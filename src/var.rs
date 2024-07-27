//! Contain abstaction of varible, function, object(struct), macro, interface(trait).

use crate::Arc;
use std::sync::Mutex;
use std::fmt::Formatter;
use std::fmt::Display;
use std::any::Any;
use std::collections::HashMap;
use crate::error::CustomError;

#[derive(Debug, Default, Clone)]
#[derive(PartialEq, Eq, Hash)]
/// record types of a varible.
pub enum VarType {
	#[default] None,
	Number,
	Bool,
	String,
	/// the `self` identifier
	SelfType,
	Result(Box<VarType>, Box<VarType>),
	Array(Box<VarType>),
	/// nablo language will not meet this type, used to refer all Array types, just like `Array<Any>`
	ArrayAny,
	Object(String),
	/// kind like rust's `dyn Trait`
	Dynamic(String),
}

impl VarType {
	/// get what current [`VarType`] look like in nablo language
	pub fn name(&self) -> String {
		match self {
			Self::Number => "num".to_string(),
			Self::Bool => "bool".to_string(),
			Self::String => "str".to_string(),
			Self::SelfType => "Self".to_string(),
			Self::Result(inner1, inner2) => format!("Result<{}, {}>", inner1.name(), inner2.name()),
			Self::Array(inner) => format!("Array<{}>", inner.name()),
			Self::Object(inner) => inner.to_string(),
			Self::Dynamic(name) => format!("dyn<{}>", name),
			Self::None => "none".to_string(),
			VarType::ArrayAny =>"Array<Any>".to_string(),
		}
	}

	/// ### checkers
	pub fn is_none(&self) -> bool {
		matches!(self, Self::None)
	}

	pub fn is_num(&self) -> bool {
		matches!(self, Self::Number)
	}

	pub fn is_self(&self) -> bool {
		matches!(self, Self::SelfType)
	}

	pub fn is_result(&self) -> bool {
		matches!(self, Self::Result(_, _))
	}

	pub fn is_array(&self) -> bool {
		matches!(self, Self::Array(_))
	}

	pub fn is_object(&self) -> bool {
		matches!(self, Self::Object(_))
	}


	pub fn is_dynamic(&self) -> bool {
		matches!(self, Self::Dynamic(_))
	}
}

impl Display for VarType {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
		write!(f, "{}", self.name())
	}
}

#[derive(Default, Clone)]
/// record value of a varible.
pub enum VarValue {
	#[default] None,
	Number(f64),
	Bool(bool),
	String(String),
	Result(Result<Box<VarValue>, Box<VarValue>>),
	Array(Vec<VarValue>),
	Object(ObjectValue),
}

impl VarValue {
	/// not precise when meeting [`VarValue::Result`]
	pub fn get_type(&self) -> VarType {
		match self {
			Self::None => {
				VarType::None
			}, 
			Self::Number(_) => {
				VarType::Number
			}, 
			Self::Bool(_) => {
				VarType::Bool
			}, 
			Self::String(_) => {
				VarType::String
			}, 
			Self::Result(val) => {
				match val {
					Ok(inner) => VarType::Result(Box::new(inner.get_type()), Box::new(VarType::Dynamic("Any".to_string()))),
					Err(inner) => VarType::Result(Box::new(VarType::Dynamic("Any".to_string())), Box::new(inner.get_type()))
				}
			}, 
			Self::Array(inner) => {
				if inner.is_empty() {
					VarType::Array(Box::new(VarType::Dynamic("Any".to_string())))
				}else {
					VarType::Array(Box::new(inner[0].get_type()))
				}
			}, 
			Self::Object(inner) => {
				VarType::Object(inner.name())
			}, 
		}
	}

	// /// clone current [`VarValue`], returns [`Option::None`] when meeting rust value
	// pub fn partial_clone(&self) -> Option<Self> {
	// 	match self {
	// 		Self::None => {
	// 			Some(Self::None)
	// 		}, 
	// 		Self::Number(inner) => {
	// 			Some(Self::Number(inner.clone()))
	// 		}, 
	// 		Self::Bool(inner) => {
	// 			Some(Self::Bool(inner.clone()))
	// 		}, 
	// 		Self::String(inner) => {
	// 			Some(Self::String(inner.clone()))
	// 		}, 
	// 		Self::Result(val) => {
	// 			match val {
	// 				Ok(inner) => Some(Self::Result(Ok(Box::new(inner.partial_clone()?)))),
	// 				Err(inner) => Some(Self::Result(Err(Box::new(inner.partial_clone()?))))
	// 			}
	// 		}, 
	// 		Self::Array(inner) => {
	// 			let mut vals = vec!();
	// 			for inside in inner {
	// 				vals.push(inside.partial_clone()?);
	// 			}
	// 			Some(Self::Array(vals))
	// 		}, 
	// 		Self::Object(inner) => {
	// 			if let ObjectValue::CustomObjectValue(val) = inner {
	// 				Some(VarValue::Object(ObjectValue::CustomObjectValue(val.partial_clone()?)))
	// 			}else {
	// 				None
	// 			}
	// 		}, 
	// 	}
	// }

	/// disply current [`VarValue`], returns [`Option::None`] when meeting rust value
	pub fn partial_display(&self) -> Option<String> {
		match self {
			Self::None => {
				Some("none".to_string())
			}, 
			Self::Number(inner) => {
				Some(inner.to_string())
			}, 
			Self::Bool(inner) => {
				Some(inner.to_string())
			}, 
			Self::String(inner) => {
				Some(inner.clone())
			}, 
			Self::Result(val) => {
				match val {
					Ok(inner) => Some(format!("Pass({})", inner.partial_display()?)),
					Err(inner) => Some(format!("Block({})", inner.partial_display()?))
				}
			}, 
			Self::Array(inner) => {
				let mut out = "[".to_string(); 
				let len = inner.len();
				for (id, inside) in inner.iter().enumerate() {
					if id + 1 == len {
						out = format!("{out}{}", inside.partial_display()?);
					}else {
						out = format!("{out}{},", inside.partial_display()?);
					}
				}
				out = format!("{out}]");
				Some(out)
			}, 
			Self::Object(inner) => {
				if let ObjectValue::CustomObjectValue(val) = inner {
					Some(val.partial_display()?)
				}else {
					None
				}
			}, 
		}
	}
}

/// An abstaction of function's code, can be used by nablo language.
#[derive(Clone)]
pub enum Code {
	/// nablo language's code
	Code(String),
	/// used to run rust function when inside nablo script
	RustFunction(RustFunction)
}

pub type RustFunction = Arc<Box<dyn Fn(&mut HashMap<String, Varible>) -> Result<Varible, CustomError>>>;

impl<T> From<T> for Code 
where
	T: Fn(&mut HashMap<String, Varible>) -> Result<Varible, CustomError> + 'static
{
	fn from(input: T) -> Self {
		Self::RustFunction(Arc::new(Box::new(input)))
	}
}

impl From<String> for Code {
	fn from(input: String) -> Self {
		Self::Code(input)
	}
}

/// An abstaction of function, can be used by nablo language.
///
/// [`Function`] can be construct from rust
/// ```
/// use nablo_programe::var::{Varible, VarType, VarValue, Function};
/// use nablo_programe::VisualMachine;
/// use std::collections::HashMap;
/// use nablo_programe::error::CustomError;
/// 
/// fn get_none(_input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
///     // will block code running if returns a `Err` value.
///     // you can get element in `input` as long as you have designated input's name
///     // let input = input.get("your_designated_name".to_string());
///     // specially the self value in method is named as `self`
///     // let self_value = input.get("self".to_string());
///     Ok(Varible { 
///         // the `name` field will be replaced to "return", so there's no need to assign that field.
///         ty: VarType::None,
///         value: VarValue::None,
///         ..Default::default()
///     }) 
/// }
///
/// assert!(Function::from(get_none).is_rust_function());
/// ```
///
/// or construct by [`crate::VisualMachine`].
/// ```
/// use nablo_programe::VisualMachine;
///
/// let code = "fn get_foo() -> str {
///     return \"foo\";
/// }";
/// let mut vm = VisualMachine::new();
/// vm.run_script(code).is_err_and(|e| panic!("{}", e));
///
/// assert!(vm.get_function("get_foo").unwrap().is_some())
/// ```
#[derive(Clone)]
pub struct Function {
	pub code: Code,
	pub name: String,
	pub input_varible: Vec<(String, VarType)>,
	pub output_varible: VarType,
	pub(crate) nesting_times: usize,
}

impl Function {
	/// creates a new empty [`Function`]
	pub fn empty() -> Self {
		Self {
			code: "".to_string().into(),
			name: String::new(),
			input_varible: vec!(),
			output_varible: VarType::None,
			nesting_times: 0,
		}
	}

	/// rename current [`Function`]
	pub fn name(self, name: impl Into<String>) -> Self {
		let name = name.into();
		Self {
			name,
			..self
		}
	}

	/// change the code of current [`Function`]
	pub fn code(self, code: impl Into<Code>) -> Self {
		let code = code.into();
		Self {
			code,
			..self
		}
	}

	/// set the input value of current [`Function`]
	pub fn input(self, input_varible: impl Into<Vec<(String, VarType)>>) -> Self {
		let input_varible = input_varible.into();
		Self {
			input_varible,
			..self
		}
	}

	/// set the output type of current [`Function`]
	pub fn output(self, output_varible: VarType) -> Self {
		Self {
			output_varible,
			..self
		}
	}

	/// check if current [`Function`] is from rust 
	pub fn is_rust_function(&self) -> bool {
		matches!(self.code, Code::RustFunction(_))
	}

	// /// clone current [`Function`], returns none if current [`Function`] is from rust.
	// pub fn partial_clone(&self) -> Option<Self> {
	// 	if let Code::Code(inner) = &self.code {
	// 		Some(Self {
	// 			code: Code::Code(inner.clone()),
	// 			name: self.name.clone(),
	// 			input_varible: self.input_varible.clone(),
	// 			output_varible: self.output_varible.clone(),
	// 			nesting_times: self.nesting_times,
	// 		})
	// 	}else {
	// 		None
	// 	}
	// }
}

impl<T> From<T> for Function 
where
	T: Into<Code>
{
	fn from(input: T) -> Self {
		Self {
			code: input.into(),
			name: String::new(),
			input_varible: vec!(),
			output_varible: VarType::None,
			nesting_times: 0,
		}
	}
}

/// An abstaction of interface(similar to rust's trait), can be used by nablo language.
#[derive(Default, Clone)]
pub struct Interface {
	pub name: String,
	pub function: HashMap<String, Function>,
}

impl Interface {
	// /// clone current function, returns none if any function is from rust.
	// pub fn partial_clone(&self) -> Option<Self> {
	// 	let mut function = HashMap::new();
	// 	for (key, func) in &self.function {
	// 		function.insert(key.clone(), func.clone());
	// 	}
	// 	Some(Self {
	// 		name: self.name.clone(),
	// 		function,
	// 	})
	// }
}

impl std::iter::FromIterator<(String, Function)> for Interface {
	fn from_iter<T>(iter: T) -> Self 
	where 
		T: IntoIterator<Item = (String, Function)>
	{
		let function = iter.into_iter().collect();
		Self {
			function,
			..Default::default()
		}
	}
}

impl<T> From<T> for Interface 
where
	T: std::iter::IntoIterator<Item = (String, Function)>
{
	fn from(input: T) -> Self {
		input.into_iter().collect()
	}
}

/// The object defination in `nablo_programe`.
///
/// Like struct in rust.
#[derive(Clone)]
pub struct CustomObject {
	pub name: String,
	pub fields: HashMap<String, VarType>,
}

/// The object value in `nablo_programe`.
///
/// Like struct in rust.
#[derive(Clone)]
pub struct CustomObjectValue {
	pub name: String,
	pub fields: HashMap<String, Varible>,
}

impl CustomObjectValue {
	// pub fn partial_clone(&self) -> Option<Self> {
	// 	let mut fields = HashMap::new();
	// 	for (name, val) in &self.fields {
	// 		fields.insert(name.clone(), val.partial_clone()?);
	// 	}
	// 	Some(Self {
	// 		name: self.name.clone(),
	// 		fields,
	// 	})
	// }

	/// disply current [`VarValue`], returns [`Option::None`] when meeting rust value
	pub fn partial_display(&self) -> Option<String> {
		let mut out = format!("{} {{\n", self.name);
		for (name, val) in &self.fields {
			out = format!("{out}    {}: {}\n", name, val.value.partial_display()?);
		}
		out = format!("{out}}}");
		Some(out)
	}
}

/// The object defination in rust.
///
/// Just use as an identifier.
#[derive(Clone)]
pub struct RustObject {
	pub name: String,
}

/// The object value in `nablo_programe`.
///
/// Just use as an identifier.
#[derive(Clone)]
pub struct RustObjectValue {
	pub name: String,
	pub origin: Arc<Mutex<Box<dyn RustyObject>>>,
}

/// any type implied this trait can be used in `nablo_language`
pub trait RustyObject: Any {
	fn type_name(&self) -> String;
	fn expose_function(&mut self) -> HashMap<String, Function> { HashMap::new() }
	fn expose_interface(&mut self) -> HashMap<String, Interface> { HashMap::new() }
}

/// An abstration of Object(struct)
#[derive(Clone)]
pub enum Object {
	CustomObject(CustomObject),
	RustObject(RustObject),
}

impl Object {
	/// get name of current object
	pub fn name(&self) -> String {
		match self {
			Object::CustomObject(inner) => inner.name.clone(),
			Object::RustObject(inner) => inner.name.clone(),
		}
	}
}

/// An abstration of Object(struct) which contains value.
#[derive(Clone)]
pub enum ObjectValue {
	CustomObjectValue(CustomObjectValue),
	RustObjectValue(RustObjectValue),
}

impl ObjectValue {
	/// get name of current object
	fn name(&self) -> String {
		match self {
			ObjectValue::CustomObjectValue(inner) => inner.name.clone(),
			ObjectValue::RustObjectValue(inner) => inner.name.clone(),
		}
	}
}

/// An abstaction of macro, can be used by nablo language.
/// [`Macro`] is a special function that recive an `Array<str>` and output `str`, the output str will be used as nablo code to run.
#[derive(Clone)]
pub struct Macro {
	pub code: Code,
	pub name: String,
	pub input_name: String,
}

impl Macro {
	/// creates a new empty [`Macro`]
	pub fn empty() -> Self {
		Self {
			code: "".to_string().into(),
			name: String::new(),
			input_name: String::new()
		}
	}

	/// rename current [`Macro`]
	pub fn name(self, name: impl Into<String>) -> Self {
		let name = name.into();
		Self {
			name,
			..self
		}
	}

	/// rename the input varible's name of current [`Macro`]
	pub fn input_name(self, input_name: impl Into<String>) -> Self {
		let input_name = input_name.into();
		Self {
			input_name,
			..self
		}
	}

	/// change the code of current [`Macro`]
	pub fn code(self, code: impl Into<Code>) -> Self {
		let code = code.into();
		Self {
			code,
			..self
		}
	}

	/// clone current function, returns none if current function is from rust.
	pub fn partial_clone(&self) -> Option<Self> {
		if let Code::Code(inner) = &self.code {
			Some(Self {
				code: Code::Code(inner.clone()),
				name: self.name.clone(),
				input_name: self.input_name.clone(),
			})
		}else {
			None
		}
	}
}

impl<T> From<T> for Macro 
where
	T: Into<Code>
{
	fn from(code: T) -> Self {
		Self {
			code: code.into(),
			..Self::empty()
		}
	}
}

#[derive(Default, Clone)]
/// An abstrction of Varible
pub struct Varible {
	pub name: String,
	pub ty: VarType,
	pub value: VarValue,
	pub nesting_times: usize
}

impl Varible {
	/// rename current [`Varible`]
	pub fn name(self, name: impl Into<String>) -> Self {
		Self {
			name: name.into(),
			..self
		}
	}
}

impl From<(VarType, VarValue)> for Varible {
	fn from(input: (VarType, VarValue)) -> Self {
		Self {
			ty: input.0,
			value: input.1,
			..Default::default()
		}
	}
}