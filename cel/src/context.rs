use crate::extensions::ExtensionRegistry;
use crate::magic::{Function, FunctionRegistry, IntoFunction};
use crate::objects::{TryIntoValue, Value};
use crate::parser::Expression;
use crate::{functions, ExecutionError};
use std::collections::BTreeMap;
use std::sync::Arc;

/// Context is a collection of variables and functions that can be used
/// by the interpreter to resolve expressions.
///
/// The context can be either a parent context, or a child context. A
/// parent context is created by default and contains all of the built-in
/// functions. A child context can be created by calling `.new_inner_scope()`. The
/// child context has it's own variables (which can be added to), but it
/// will also reference the parent context. This allows for variables to
/// be overridden within the child context while still being able to
/// resolve variables in the child's parents. You can have theoretically
/// have an infinite number of child contexts that reference each-other.
///
/// So why is this important? Well some CEL-macros such as the `.map` macro
/// declare intermediate user-specified identifiers that should only be
/// available within the macro, and should not override variables in the
/// parent context. The `.map` macro can create a child context from the parent, add the
/// intermediate identifier to the child context, and then evaluate the
/// map expression.
///
/// Intermediate variable stored in child context
///               ↓
/// [1, 2, 3].map(x, x * 2) == [2, 4, 6]
///                  ↑
/// Only in scope for the duration of the map expression
///
pub enum Context<'a> {
    Root {
        functions: FunctionRegistry,
        variables: BTreeMap<String, Value>,
        resolver: Option<&'a dyn VariableResolver>,
        extensions: ExtensionRegistry,
        container: Option<String>,
    },
    Child {
        parent: &'a Context<'a>,
        variables: BTreeMap<String, Value>,
        resolver: Option<&'a dyn VariableResolver>,
        container: Option<String>,
    },
}

impl<'a> Context<'a> {
    pub fn add_variable<S, V>(
        &mut self,
        name: S,
        value: V,
    ) -> Result<(), <V as TryIntoValue>::Error>
    where
        S: Into<String>,
        V: TryIntoValue,
    {
        match self {
            Context::Root { variables, .. } => {
                variables.insert(name.into(), value.try_into_value()?);
            }
            Context::Child { variables, .. } => {
                variables.insert(name.into(), value.try_into_value()?);
            }
        }
        Ok(())
    }

    pub fn add_variable_from_value<S, V>(&mut self, name: S, value: V)
    where
        S: Into<String>,
        V: Into<Value>,
    {
        match self {
            Context::Root { variables, .. } => {
                variables.insert(name.into(), value.into());
            }
            Context::Child { variables, .. } => {
                variables.insert(name.into(), value.into());
            }
        }
    }

    pub fn set_variable_resolver(&mut self, r: &'a dyn VariableResolver) {
        match self {
            Context::Root { resolver, .. } => {
                *resolver = Some(r);
            }
            Context::Child { resolver, .. } => {
                *resolver = Some(r);
            }
        }
    }

    pub fn get_variable<S>(&self, name: S) -> Result<Value, ExecutionError>
    where
        S: AsRef<str>,
    {
        let name = name.as_ref();
        match self {
            Context::Child {
                variables,
                parent,
                resolver,
                container,
            } => resolver
                .and_then(|r| r.resolve(name))
                .or_else(|| {
                    variables
                        .get(name)
                        .cloned()
                        .or_else(|| parent.get_variable(name).ok())
                })
                .or_else(|| {
                    // Try qualified name resolution with container
                    if let Some(container_name) = container {
                        self.try_qualified_lookup(name, container_name, variables, Some(parent))
                    } else {
                        None
                    }
                })
                .ok_or_else(|| ExecutionError::UndeclaredReference(name.to_string().into())),
            Context::Root {
                variables,
                resolver,
                container,
                ..
            } => resolver
                .and_then(|r| r.resolve(name))
                .or_else(|| variables.get(name).cloned())
                .or_else(|| {
                    // Try qualified name resolution with container
                    if let Some(container_name) = container {
                        self.try_qualified_lookup(name, container_name, variables, None)
                    } else {
                        None
                    }
                })
                .ok_or_else(|| ExecutionError::UndeclaredReference(name.to_string().into())),
        }
    }

    /// Attempts to resolve a variable name using qualified name resolution.
    ///
    /// According to the CEL spec, when a container is set, identifiers should be resolved
    /// by trying progressively shorter prefixes. For example, if the container is "a.b.c"
    /// and we're looking for identifier "x", we should try:
    /// 1. "x" (already tried in get_variable)
    /// 2. "a.b.c.x"
    /// 3. "a.b.x"
    /// 4. "a.x"
    fn try_qualified_lookup(
        &self,
        name: &str,
        container: &str,
        variables: &BTreeMap<String, Value>,
        parent: Option<&Context<'_>>,
    ) -> Option<Value> {
        // Build a list of candidate names to try
        let mut candidates = Vec::new();

        // Add the fully qualified name
        candidates.push(format!("{}.{}", container, name));

        // Add progressively shorter prefixes
        let parts: Vec<&str> = container.split('.').collect();
        for i in (1..parts.len()).rev() {
            let prefix = parts[..i].join(".");
            candidates.push(format!("{}.{}", prefix, name));
        }

        // Try each candidate
        for candidate in candidates {
            // Check in current context's variables
            if let Some(value) = variables.get(&candidate) {
                return Some(value.clone());
            }

            // Check in parent context if available
            if let Some(parent) = parent {
                if let Ok(value) = parent.get_variable(&candidate) {
                    return Some(value);
                }
            }
        }

        None
    }

    pub fn get_extension_registry(&self) -> Option<&ExtensionRegistry> {
        match self {
            Context::Root { extensions, .. } => Some(extensions),
            Context::Child { parent, .. } => parent.get_extension_registry(),
        }
    }

    pub fn get_extension_registry_mut(&mut self) -> Option<&mut ExtensionRegistry> {
        match self {
            Context::Root { extensions, .. } => Some(extensions),
            Context::Child { .. } => None,
        }
    }

    pub(crate) fn get_function(&self, name: &str) -> Option<&Function> {
        // First try direct lookup
        let direct = match self {
            Context::Root { functions, .. } => functions.get(name),
            Context::Child { parent, .. } => parent.get_function(name),
        };

        if direct.is_some() {
            return direct;
        }

        // Try qualified name resolution with container
        let container_name = self.get_container()?;

        // Build a list of candidate names to try
        let mut candidates = Vec::new();

        // Add the fully qualified name
        candidates.push(format!("{}.{}", container_name, name));

        // Add progressively shorter prefixes
        let parts: Vec<&str> = container_name.split('.').collect();
        for i in (1..parts.len()).rev() {
            let prefix = parts[..i].join(".");
            candidates.push(format!("{}.{}", prefix, name));
        }

        // Try each candidate
        for candidate in &candidates {
            match self {
                Context::Root { functions, .. } => {
                    if let Some(func) = functions.get(candidate) {
                        return Some(func);
                    }
                }
                Context::Child { parent, .. } => {
                    if let Some(func) = parent.get_function(candidate) {
                        return Some(func);
                    }
                }
            }
        }

        None
    }

    pub fn add_function<T: 'static, F>(&mut self, name: &str, value: F)
    where
        F: IntoFunction<T> + 'static + Send + Sync,
    {
        if let Context::Root { functions, .. } = self {
            functions.add(name, value);
        };
    }

    pub fn resolve(&self, expr: &Expression) -> Result<Value, ExecutionError> {
        Value::resolve(expr, self)
    }

    pub fn resolve_all(&self, exprs: &[Expression]) -> Result<Value, ExecutionError> {
        Value::resolve_all(exprs, self)
    }

    pub fn new_inner_scope(&self) -> Context<'_> {
        Context::Child {
            parent: self,
            variables: Default::default(),
            resolver: None,
            container: None,
        }
    }

    pub fn with_container(mut self, container: String) -> Self {
        match &mut self {
            Context::Root { container: c, .. } => {
                *c = Some(container);
            }
            Context::Child { container: c, .. } => {
                *c = Some(container);
            }
        }
        self
    }

    pub fn get_container(&self) -> Option<&str> {
        match self {
            Context::Root { container, .. } => container.as_deref(),
            Context::Child { container, parent, .. } => {
                container.as_deref().or_else(|| parent.get_container())
            }
        }
    }

    /// Constructs a new empty context with no variables or functions.
    ///
    /// If you're looking for a context that has all the standard methods, functions
    /// and macros already added to the context, use [`Context::default`] instead.
    ///
    /// # Example
    /// ```
    /// use cel::Context;
    /// let mut context = Context::empty();
    /// context.add_function("add", |a: i64, b: i64| a + b);
    /// ```
    pub fn empty() -> Self {
        Context::Root {
            variables: Default::default(),
            functions: Default::default(),
            resolver: None,
            extensions: ExtensionRegistry::new(),
            container: None,
        }
    }
}

impl Default for Context<'_> {
    fn default() -> Self {
        let mut ctx = Context::Root {
            variables: Default::default(),
            functions: Default::default(),
            resolver: None,
            extensions: ExtensionRegistry::new(),
            container: None,
        };

        ctx.add_function("contains", functions::contains);
        ctx.add_function("size", functions::size);
        ctx.add_function("max", functions::max);
        ctx.add_function("min", functions::min);
        ctx.add_function("startsWith", functions::starts_with);
        ctx.add_function("endsWith", functions::ends_with);
        ctx.add_function("string", functions::string);
        ctx.add_function("bytes", functions::bytes);
        ctx.add_function("double", functions::double);
        ctx.add_function("float", functions::float);
        ctx.add_function("int", functions::int);
        ctx.add_function("uint", functions::uint);
        ctx.add_function("optional.none", functions::optional_none);
        ctx.add_function("optional.of", functions::optional_of);
        ctx.add_function(
            "optional.ofNonZeroValue",
            functions::optional_of_non_zero_value,
        );
        ctx.add_function("value", functions::optional_value);
        ctx.add_function("hasValue", functions::optional_has_value);
        ctx.add_function("or", functions::optional_or_optional);
        ctx.add_function("orValue", functions::optional_or_value);

        #[cfg(feature = "regex")]
        ctx.add_function("matches", functions::matches);

        #[cfg(feature = "chrono")]
        {
            ctx.add_function("duration", functions::duration);
            ctx.add_function("timestamp", functions::timestamp);
            ctx.add_function("getFullYear", functions::time::timestamp_year);
            ctx.add_function("getMonth", functions::time::timestamp_month);
            ctx.add_function("getDayOfYear", functions::time::timestamp_year_day);
            ctx.add_function("getDayOfMonth", functions::time::timestamp_month_day);
            ctx.add_function("getDate", functions::time::timestamp_date);
            ctx.add_function("getDayOfWeek", functions::time::timestamp_weekday);
            ctx.add_function("getHours", functions::time::timestamp_hours);
            ctx.add_function("getMinutes", functions::time::timestamp_minutes);
            ctx.add_function("getSeconds", functions::time::timestamp_seconds);
            ctx.add_function("getMilliseconds", functions::time::timestamp_millis);
        }

        ctx
    }
}

/// VariableResolver implements a custom resolver for variables that is consulted before looking at
/// variables added to the context. This allows dynamic variables, or avoiding HashMap lookup/creation.
///
///
/// # Example
/// ```
/// struct ValueContext {
///     request: cel::Value,
///     response: cel::Value,
/// }
///
/// impl cel::context::VariableResolver for ValueContext {
///     fn resolve(&self, variable: &str) -> Option<cel::Value> {
///         match variable {
///             "request" => Some(self.request.clone()),
///             "response" => Some(self.response.clone()),
///             _ => None,
///         }
///     }
/// }
/// ```
pub trait VariableResolver: Send + Sync {
    fn resolve(&self, variable: &str) -> Option<Value>;
}

impl<T: VariableResolver> VariableResolver for Box<T> {
    fn resolve(&self, variable: &str) -> Option<Value> {
        (**self).resolve(variable)
    }
}

impl<T: VariableResolver> VariableResolver for Arc<T> {
    fn resolve(&self, variable: &str) -> Option<Value> {
        (**self).resolve(variable)
    }
}

impl<T: VariableResolver> VariableResolver for &T {
    fn resolve(&self, variable: &str) -> Option<Value> {
        (**self).resolve(variable)
    }
}
