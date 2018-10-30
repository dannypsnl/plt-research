#[derive(Debug, PartialEq)]
// isPointer, type name
pub struct Type(pub bool, pub String);
#[derive(Debug, PartialEq)]
pub struct Parameter(pub Type, pub String);
#[derive(Debug, PartialEq)]
pub enum Top {
    // Type Name
    Func(Type, String, Vec<Parameter>),
}
