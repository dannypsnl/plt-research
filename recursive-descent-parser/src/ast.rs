#[derive(Debug, PartialEq)]
pub enum Type {
    Normal(String),
    Pointer(Box<Type>),
}
#[derive(Debug, PartialEq)]
pub struct Parameter(pub Type, pub String);
#[derive(Debug, PartialEq)]
pub enum Top {
    /// Func: Type, Name, Parameters
    Func(Type, String, Vec<Parameter>),
}
