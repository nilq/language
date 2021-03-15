use std::collections::HashMap;
use crate::parser::ast::Binding;

pub struct Scope {
    depth: usize,
    function_depth: usize,
    pub variables: HashMap<String, Binding>,
}

impl Scope {
    pub fn root() -> Self {
        Scope {
            depth: 0,
            function_depth: 0,
            variables: HashMap::new()
        }
    }

    pub fn new(depth: usize, function_depth: usize) -> Self {
        Scope {
            depth,
            function_depth,
            variables: HashMap::new()
        }
    }

    pub fn child(&self, is_function: bool) -> Self {
        Scope {
            depth: self.depth + 1,
            function_depth: self.function_depth + is_function as usize,
            variables: HashMap::new()
        }
    }

    pub fn local(&self, name: String) -> Binding {
        Binding::local(name.clone(), self.depth, self.function_depth)
    }

    pub fn assign_local(&mut self, name: String) -> Binding {
        let binding = Binding::local(name.clone(), self.depth, self.function_depth);

        self.variables.insert(
            name,
            binding.clone()
        );

        binding
    }

    pub fn get(&self, name: &String) -> Option<&Binding> {
        self.variables.get(name)
    }
}

pub struct SymTab {
    pub scopes: Vec<Scope>,
    pub globals: HashMap<String, Binding>
}

impl SymTab {
    pub fn new() -> Self {
        SymTab {
            scopes: vec!(Scope::root()),
            globals: HashMap::new()
        }
    }

    pub fn local(&mut self, name: String) -> Binding {
        self.top_mut().local(name)
    }

    pub fn assign_local(&mut self, name: String) -> Binding {
        self.top_mut().assign_local(name)
    }

    pub fn assign_global(&mut self, name: String) -> Binding {
        let binding = Binding::global(name.clone());

        self.globals.insert(
            name,
            binding.clone()
        );

        binding
    }

    pub fn get(&self, name: &String) -> Option<&Binding> {
        if let Some(binding) = self.globals.get(name) {
            Some(binding)
        } else {
            let mut i = self.scopes.len() - 1;
            loop {
                let scope = &self.scopes[i];

                if let Some(binding) = scope.get(name) {
                    return Some(binding)
                }

                if i == 0 {
                    return None
                } else {
                    i -= 1
                }
            }
        }
    }

    pub fn enter(&mut self) {
        let scope = self.top().child(false);
        self.scopes.push(scope)
    }

    pub fn enter_tmp(&mut self) {
        let scope = self.top().child(false);
        self.scopes.push(scope)
    }

    pub fn enter_func(&mut self) {
        let scope = self.top().child(true);
        self.scopes.push(scope)
    }

    pub fn yeet(&mut self) {
        self.scopes.pop();
    }

    pub fn current_depth(&self) -> usize {
        self.top().depth
    }

    fn top(&self) -> &Scope {
        self.scopes.last().unwrap()
    }

    fn top_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }
}