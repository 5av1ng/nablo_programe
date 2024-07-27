/*! `nablo_programe` is a lib aim to provide middle layer to link shader and rust.
 * 
 * We're using a extramely simplicated rust for middle language and using binary code to send datas to wgpu's shader.
 * The grammer will be diffenert from normal script.
 * The detailed Coding system see consts floor.
 * 
 * Also `nablo_programe` can be used as a normal script language. 
 * You can learn the grammer at `grammer.md`
 * 
 * `nablo_programe` have a highly readable error info if you displayed it by `format!("{}", err)`.
 **/
#![allow(clippy::arc_with_non_send_sync)]

pub mod var;
pub mod error;
pub mod specific_function;
// pub mod shader_version;

mod standard_lib;

lazy_static::lazy_static! {
	static ref PRATT_PARSER: PrattParser<Rule> = {
		use pest::pratt_parser::{Assoc, Op};

		// Precedence is defined lowest to highest
		PrattParser::new()
			// Addition and subtract have equal precedence
			.op(Op::infix(Rule::LargeThan, Assoc::Left) | Op::infix(Rule::LessThan, Assoc::Left) | Op::infix(Rule::LargerOrEqualThan, Assoc::Left) | Op::infix(Rule::LessOrEqualThan, Assoc::Left) | Op::infix(Rule::Unequal, Assoc::Left) | Op::infix(Rule::Same, Assoc::Left))
			.op(Op::infix(Rule::And, Assoc::Left) | Op::infix(Rule::Or, Assoc::Left))
			.op(Op::infix(Rule::Add, Assoc::Left) | Op::infix(Rule::Minus, Assoc::Left))
			.op(Op::infix(Rule::Cross, Assoc::Left) | Op::infix(Rule::Divid, Assoc::Left))
			.op(Op::infix(Rule::Power, Assoc::Right))
	};
}

use pest::pratt_parser::PrattParser;
use log::*;
use std::collections::HashSet;
use std::ops::BitOr;
use std::sync::Mutex;
use std::sync::Arc;
use pest::iterators::Pair;
use std::collections::HashMap;
use pest::Parser;
use pest_derive::Parser;
use crate::var::*;
use crate::error::*;

/// the interface that will automatically implied for all type of value.
pub(crate) const AUTO_INTERFACE: [&str; 1] = ["Any"];

#[derive(Parser)]
#[grammar = "./grammer.pest"]
struct NabloParser;

#[derive(Debug)]
enum Operation {
	Add,
	Minus,
	Cross,
	Divid,
	Power,
	LargerOrEqualThan,
	LessOrEqualThan,
	LargeThan,
	LessThan,
	Unequal,
	Same,
	And,
	Or,
}

struct Caculation {
	tree: CaculationTree,
	get_value: Option<Box<Caculation>>,
	visit_field: Option<String>,
	visit_function: Option<(String, Vec<Caculation>)>,
}

impl Default for Caculation {
	fn default() -> Self {
		Self {
			tree: CaculationTree::TempValue(VarType::None, VarValue::None),
			get_value: None,
			visit_function: None,
			visit_field: None,
		}
	}
}

enum CaculationTree {
	TempValue(VarType, VarValue),
	MethodUse {
		ty: VarType,
		total_code: String,
	},
	FunctionUse { 
		total_code: String,
	},
	Value(String),
	Op {
		lhs: Box<Caculation>,
		op: Operation,
		rhs: Box<Caculation>,
	},
	PreOp {
		// false for not
		is_neg: bool,
		rhs: Box<Caculation>,
	}
}

type ImpliedElement = HashMap<VarType, (HashMap<String, Function>, HashMap<String, Interface>)>;

/// Here's where you run your script and get the result from script.
///
/// You can use registrate_* to registrate element into current [`VisualMachine`].
/// You can use run_* to run exist [`Function`] or code.
/// [`VisualMachine`] should be cheap to clone. 
/// 
/// [`VisualMachine::default()`] is completly unusable, kept for further custom usage.
pub struct VisualMachine {
	implied_interfaces: Arc<Mutex<ImpliedElement>>,
	regstrated_objects: Arc<Mutex<HashMap<String, Object>>>,
	regstrated_interfaces: Arc<Mutex<HashMap<String, Interface>>>,
	regstrated_functions: Arc<Mutex<HashMap<String, Function>>>,
	regstrated_macros: Arc<Mutex<HashMap<String, Macro>>>,
	used_varibles: Arc<Mutex<HashMap<String, Varible>>>,
	nesting_times: usize,
	current_self_type: Option<VarType>,
	loop_times: usize,
	return_value_type: Option<VarType>,
	current_function_name: Option<String>
}

impl Default for VisualMachine {
	fn default() -> Self {
		let implied_interfaces = Arc::new(Mutex::new(HashMap::from([
			(VarType::None, (HashMap::new(), HashMap::new())),
			(VarType::Number, (HashMap::new(), HashMap::new())),
			(VarType::Bool, (HashMap::new(), HashMap::new())),
			(VarType::String, (HashMap::new(), HashMap::new())),
			(VarType::ArrayAny, (HashMap::new(), HashMap::new())),
		])));
		Self {
			implied_interfaces,
			regstrated_objects: Default::default(),
			regstrated_interfaces: Default::default(),
			regstrated_functions: Default::default(),
			regstrated_macros: Default::default(),
			used_varibles: Default::default(),
			nesting_times: Default::default(),
			current_self_type: Default::default(),
			loop_times: Default::default(),
			return_value_type: Default::default(),
			current_function_name: Default::default(),
		}
	}
}

impl Clone for VisualMachine {
	fn clone(&self) -> Self {
		Self {
			implied_interfaces: self.implied_interfaces.clone(),
			regstrated_objects: self.regstrated_objects.clone(),
			regstrated_interfaces: self.regstrated_interfaces.clone(),
			regstrated_functions: self.regstrated_functions.clone(),
			regstrated_macros: self.regstrated_macros.clone(),
			used_varibles: Arc::new(Mutex::new(HashMap::new())),
			nesting_times: self.nesting_times + 1,
			current_self_type: self.current_self_type.clone(),
			loop_times: 0,
			return_value_type: self.return_value_type.clone(),
			current_function_name: None,
		}
	}
}

#[derive(Clone)]
#[derive(Debug)]
enum Condition {
	Return,
	Continue,
	Break,
	None
}

impl BitOr<Condition> for Condition {
	type Output = Condition;
	fn bitor(self, rhs: Condition) -> Condition {
		match rhs {
			Self::None => self,
			_ => rhs,
		}
	}
}

impl VisualMachine {
	/// get a [`VisualMachine`] with full standard library
	///
	/// See what standard library at [`Self::standard_lib()`]
	pub fn new() -> Self {
		Self::standard_lib()
	}

	/// get a empty [`VisualMachine`], completly unusable, kept for further custom usage.
	pub fn empty() -> Self {
		Self::default()
	}

	/// Run a script, after running, the [`VisualMachine`] will save all defined elements except dropped [`Varible`] and [`Function`].
	///
	/// Note: [`VisualMachine`] will stop while met error, but keeps the state before stop.  
	pub fn run_script(&mut self, input: impl Into<String>) -> Result<(), Error> {
		let change_err = |error: Error, current_function_name: Option<String>| -> Error {
			if error.function.is_none() {
				Error {
					function: current_function_name,
					..error
				}
			}else {
				error
			}
		};
		let input = input.into();
		let mut code = String::new();
		for line in input.lines() {
			let new_line = line.trim();
			if let Some(inner) = new_line.get(0..2) {
				if inner == "//" {
					code = format!("{code}\n");
				}else {
					code = format!("{code}{}\n", new_line);
				}
			}else {
				code = format!("{code}{}\n", new_line);
			}
		}
		let mut current_line = 1;
		if let Err((code, error_line, error_col, error_length)) = self.check_ascii(&code) {
			return Err(Error {
				error_type: ErrorType::NonAsciiInput,
				code,
				start_line: error_line,
				error_line,
				error_col,
				error_length,
				function: self.current_function_name.clone(),
			});
		} 
		debug!("checking grammer mistakes...");
		loop {
			let mut rest_code: String = code.lines().enumerate().map(|(i, text)| {
				if (i + 1) >= current_line {
					text.to_owned() + "\n"
				}else {
					"".to_string()
				}
			}).collect();
			let mut i = 0;
			loop {
				if i >= rest_code.len() {
					break;
				}
				let chara = rest_code.get(i..i+1).unwrap().chars().next().unwrap();
				if chara == '\n' {
					current_line += 1;
				}else if !chara.is_whitespace() {
					break;
				}
				i += 1;
			}
			rest_code = rest_code.trim_start().to_string();
			if rest_code.is_empty() {
				break;
			}
			let parsed_data = match NabloParser::parse(Rule::Statement, &rest_code) {
				Ok(t) => t,
				Err(e) => return Err(change_err((e, &rest_code).into(), self.current_function_name.clone()))
			}.next();
			if let Some(parsed_data) = parsed_data {
				let length = parsed_data.as_str().len();
				current_line += rest_code[0..length].lines().count();
			}
		}

		debug!("running script...");
		let parsed_data = match NabloParser::parse(Rule::Statements, &code) {
			Ok(t) => t,
			Err(e) => return Err(change_err((e, &code).into(), self.current_function_name.clone()))
		}.next();
		if let Some(parsed_data) = parsed_data {
			for parsed_data in parsed_data.into_inner() {
				if let Err(e) = self.handle_statement(&parsed_data) { 
					return Err(change_err(e, self.current_function_name.clone())) 
				};
			}
		}else {
			return Err(Error {
				function: self.current_function_name.clone(),
				..Error::new_empty(ErrorType::NoInput)
			});
		}
		debug!("finished running script");
		Ok(())
	}

	/// Run a [`Function`], just as what in nablo language.
	///
	/// Note: cant run function with `Self`, to run that, use [`Self::run_method`] instead.
	pub fn run_function(&mut self, mut input_varible: HashMap<String, Varible>, function: Function) -> Result<Varible, Error> {
		let mut sub_vm = self.clone();
		input_varible.insert("return".to_string(), Varible {
			name: "return".to_string(),
			ty: VarType::None,
			value: VarValue::None,
			nesting_times: 0,
		});
		let mut code = format!("{}(", function.name);
		for key in input_varible.keys() {
			code = format!("{code}{key}, ");
		}
		code = format!("{code});");
		sub_vm.used_varibles = Arc::new(Mutex::new(input_varible));
		sub_vm.return_value_type = Some(function.output_varible.clone());
		sub_vm.current_function_name = Some(function.name.clone());
		sub_vm.run_script(code)?;
		let mut used_varibles = sub_vm.used_varibles.lock()?;
		Ok(used_varibles.remove("return").unwrap())
	}

	/// Run a [`Function`] as method, just as what in nablo language.
	///
	/// Note: you need designate `self` as "self".to_string() if method was declared as `fn name(self, /*other parameter*/)`.
	pub fn run_method(&mut self, mut input_varible: HashMap<String, Varible>, function: Function, self_type: &VarType) -> Result<Varible, Error> {
		let mut sub_vm = self.clone();
		input_varible.insert("return".to_string(), Varible {
			name: "return".to_string(),
			ty: VarType::None,
			value: VarValue::None,
			nesting_times: 0,
		});
		let mut code = format!("{}(", function.name);
		for key in input_varible.keys() {
			code = format!("{code}{key}, ");
		}
		code = format!("{code});");
		sub_vm.used_varibles = Arc::new(Mutex::new(input_varible));
		sub_vm.current_self_type = Some(self_type.clone());
		sub_vm.return_value_type = Some(function.output_varible.clone());
		sub_vm.current_function_name = Some(function.name.clone());
		sub_vm.run_script(code)?;
		let mut used_varibles = sub_vm.used_varibles.lock()?;
		Ok(used_varibles.remove("return").unwrap())
	}

	/// Regstrate a new [`Object`] to current [`VisualMachine`]. 
	///
	/// Like object statement in nablo language, but ignored regstrated one.
	///
	/// Return orginal [`Object`](if any).
	///
	/// Return error when current [`VisualMachine`]'s mutex is poisoned.
	pub fn regstrate_object(&self, object: impl Into<Object>) -> Result<Option<Object>, Error> {
		let object = object.into();
		self.implied_interfaces.lock()?.insert(VarType::Object(object.name().clone()), (HashMap::new(), HashMap::from([("Any".to_string(), Interface::any())])));
		Ok(self.regstrated_objects.lock()?.insert(object.name().clone(), object))
	}

	/// Get a [`Object`] from current [`VisualMachine`]'s registrated objects.
	///
	/// Return error when current [`VisualMachine`]'s mutex is poisoned.
	pub fn get_object(&self, object_name: impl Into<String>) -> Result<Option<Object>, Error> {
		let object_name = object_name.into();
		let regstrated_objects = self.regstrated_objects.lock()?;
		Ok(regstrated_objects.get(&object_name).cloned())
	}

	/// Remove a [`Object`] from current [`VisualMachine`]'s completly.
	///
	/// Return orginal [`Object`](if any).
	///
	/// Return error when current [`VisualMachine`]'s mutex is poisoned.
	pub fn remove_object(&self, object_name: impl Into<String>) -> Result<Option<Object>, Error> {
		let object_name = object_name.into();
		self.implied_interfaces.lock()?.remove(&VarType::Object(object_name.to_string()));
		let mut regstrated_objects = self.regstrated_objects.lock()?;
		Ok(regstrated_objects.remove(&object_name))
	}

	/// Regstrate a new [`Function`] to current [`VisualMachine`]. 
	///
	/// Like function statement in nablo language, but ignored regstrated one.
	///
	/// Return orginal [`Function`](if any).
	///
	/// Return error when current [`VisualMachine`]'s mutex is poisoned.
	pub fn regstrate_function(&self, function: impl Into<Function>) -> Result<Option<Function>, Error> {
		let function = function.into();
		Ok(self.regstrated_functions.lock()?.insert(function.name.clone(), function))
	}

	/// Get a [`Function`] from current [`VisualMachine`]'s registrated functions. 
	///
	/// Return error when current [`VisualMachine`]'s mutex is poisoned.
	pub fn get_function(&self, function_name: impl Into<String>) -> Result<Option<Function>, Error> {
		let function_name = function_name.into();
		let regstrated_functions = self.regstrated_functions.lock()?;
		Ok(regstrated_functions.get(&function_name).cloned())
	}

	/// Remove a [`Function`] from current [`VisualMachine`]'s registrated functions. 
	///
	/// Return orginal [`Function`](if any).
	///
	/// Return error when current [`VisualMachine`]'s mutex is poisoned.
	pub fn remove_function(&self, function_name: impl Into<String>) -> Result<Option<Function>, Error> {
		let function_name = function_name.into();
		let mut regstrated_functions = self.regstrated_functions.lock()?;
		Ok(regstrated_functions.remove(&function_name))
	}

	/// Regstrate a new [`Interface`] to current [`VisualMachine`]. 
	///
	/// Like interface statement in nablo language, but ignored regstrated one.
	/// If the [`Interface`]'s `code` field is [`Code::Code`], the code inside will be ignored.
	///
	/// Return orginal [`Interface`](if any).
	///
	/// Return error when current [`VisualMachine`]'s mutex is poisoned.
	pub fn regstrate_interface(&self, interface: impl Into<Interface>) -> Result<Option<Interface>, Error> {
		let interface = interface.into();
		Ok(self.regstrated_interfaces.lock()?.insert(interface.name.clone(), interface))
	}

	/// Get a [`Interface`] from current [`VisualMachine`]'s registrated interfaces. 
	///
	/// Return error when current [`VisualMachine`]'s mutex is poisoned.
	pub fn get_interface(&self, interface_name: impl Into<String>) -> Result<Option<Interface>, Error> {
		Ok(self.regstrated_interfaces.lock()?.get(&interface_name.into()).cloned())
	}

	/// Remove a [`Interface`] from current [`VisualMachine`]'s registrated interfaces. 
	///
	/// Return orginal [`Interface`](if any).
	///
	/// Return error when current [`VisualMachine`]'s mutex is poisoned.
	pub fn remove_interface(&self, interface_name: impl Into<String>) -> Result<Option<Interface>, Error> {
		Ok(self.regstrated_interfaces.lock()?.remove(&interface_name.into()))
	}

	/// Regstrate a new [`Macro`] to current [`VisualMachine`]. 
	///
	/// Like macro statement in nablo language, but ignored regstrated one.
	///
	/// Return orginal [`Macro`](if any).
	///
	/// Return error when current [`VisualMachine`]'s mutex is poisoned.
	pub fn regstrate_macro(&self, the_macro: impl Into<Macro>) -> Result<Option<Macro>, Error> {
		let the_macro = the_macro.into();
		Ok(self.regstrated_macros.lock()?.insert(the_macro.name.clone(), the_macro))
	}

	/// Get a [`Macro`] from current [`VisualMachine`]'s registrated functions. 
	///
	/// Return error when current [`VisualMachine`]'s mutex is poisoned.
	pub fn get_macro(&self, macro_name: impl Into<String>) -> Result<Option<Macro>, Error> {
		Ok(self.regstrated_macros.lock()?.get(&macro_name.into()).cloned())
	}

	/// Regstrate a new method to given [`VarType`] on current [`VisualMachine`]. 
	///
	/// Like impliment statement in nablo language, but ignored regstrated one.
	/// You need add `self` input varible manually to let the function able to visit `self`
	///
	/// Return orginal method(if any).
	///
	/// Return error when given type is not exist, try to imply a dynamic type or current [`VisualMachine`]'s mutex is poisoned, which shouldn't happen for now.
	pub fn imply_method(&self, type_to_imply: &VarType, method: impl Into<Function>) -> Result<Option<Function>, Error> {
		if type_to_imply.is_dynamic() {
			return Err(ErrorType::UnsupportImply.into()); 
		}
		let mut implied_interfaces = self.implied_interfaces.lock()?;
		let method = method.into();
		if let Some((regstrated_method, _)) = implied_interfaces.get_mut(type_to_imply) {
			Ok(regstrated_method.insert(method.name.clone(), method))
		}else {
			Err(ErrorType::TypeNotFound(type_to_imply.to_string()).into())
		}
	}

	/// Get a exist method on given [`VarType`] from current [`VisualMachine`]. 
	///
	/// Return error when given type is not exist, try to imply a dynamic type or current [`VisualMachine`]'s mutex is poisoned, which shouldn't happen for now.
	pub fn get_method(&self, type_to_imply: &VarType, method: impl Into<String>) -> Result<Option<Function>, Error> {
		if type_to_imply.is_dynamic() {
			return Err(ErrorType::UnsupportImply.into()); 
		}
		let mut implied_interfaces = self.implied_interfaces.lock()?;
		let method = method.into();
		if let Some((regstrated_method, _)) = implied_interfaces.get_mut(type_to_imply) {
			Ok(regstrated_method.get(&method.clone()).cloned())
		}else {
			Err(ErrorType::TypeNotFound(type_to_imply.to_string()).into())
		}
	}

	/// Remove a exist method on given [`VarType`] from current [`VisualMachine`]. 
	///
	/// Return orginal method(if any).
	///
	/// Return error when given type is not exist, try to imply a dynamic type or current [`VisualMachine`]'s mutex is poisoned, which shouldn't happen for now.
	pub fn remove_method(&self, type_to_imply: &VarType, method: impl Into<String>) -> Result<Option<Function>, Error> {
		if type_to_imply.is_dynamic() {
			return Err(ErrorType::UnsupportImply.into()); 
		}
		let mut implied_interfaces = self.implied_interfaces.lock()?;
		let method = method.into();
		if let Some((regstrated_method, _)) = implied_interfaces.get_mut(type_to_imply) {
			Ok(regstrated_method.remove(&method.clone()))
		}else {
			Err(ErrorType::TypeNotFound(type_to_imply.to_string()).into())
		}
	}

	/// Regstrate a new [`Interface`] to given [`VarType`] on current [`VisualMachine`]. 
	///
	/// Like impliment statement in nablo language, but ignored regstrated one.
	///
	/// Return orginal [`Interface`](if any).
	///
	/// Return error when given type is not exist, try to imply a dynamic type or current [`VisualMachine`]'s mutex is poisoned, which shouldn't happen for now.
	pub fn imply_interface(&self, type_to_imply: &VarType, interface: impl Into<Interface>) -> Result<Option<Interface>, Error> {
		if type_to_imply.is_dynamic() {
			return Err(ErrorType::UnsupportImply.into()); 
		}
		let mut implied_interfaces = self.implied_interfaces.lock()?;
		let interface = interface.into();
		if let Some((_, regstrated_interfaces)) = implied_interfaces.get_mut(type_to_imply) {
			Ok(regstrated_interfaces.insert(interface.name.clone(), interface))
		}else {
			Err(ErrorType::TypeNotFound(type_to_imply.to_string()).into())
		}
	}

	/// Get an exist [`Interface`] on given [`VarType`] from current [`VisualMachine`]. 
	///
	/// Return error when given type is not exist, try to imply a dynamic type or current [`VisualMachine`]'s mutex is poisoned, which shouldn't happen for now.
	pub fn get_implied_interface(&self, type_imply: &VarType, interface: impl Into<String>) -> Result<Option<Interface>, Error> {
		if type_imply.is_dynamic() {
			return Err(ErrorType::UnsupportImply.into()); 
		}
		let mut implied_interfaces = self.implied_interfaces.lock()?;
		let interface = interface.into();
		if let Some((_, regstrated_interfaces)) = implied_interfaces.get_mut(type_imply) {
			Ok(regstrated_interfaces.get(&interface.clone()).cloned())
		}else {
			Err(ErrorType::TypeNotFound(type_imply.to_string()).into())
		}
	}

	/// Remove an exist [`Interface`] on given [`VarType`] from current [`VisualMachine`]. 
	///
	/// Return orginal [`Interface`](if any).
	///
	/// Return error when given type is not exist, try to imply a dynamic type or current [`VisualMachine`]'s mutex is poisoned, which shouldn't happen for now.
	pub fn remove_implied_interface(&self, type_imply: &VarType, interface: impl Into<String>) -> Result<Option<Interface>, Error> {
		if type_imply.is_dynamic() {
			return Err(ErrorType::UnsupportImply.into()); 
		}
		let mut implied_interfaces = self.implied_interfaces.lock()?;
		let interface = interface.into();
		if let Some((_, regstrated_interfaces)) = implied_interfaces.get_mut(type_imply) {
			Ok(regstrated_interfaces.remove(&interface.clone()))
		}else {
			Err(ErrorType::TypeNotFound(type_imply.to_string()).into())
		}
	}

	/// Inteset a new [`Varible`] into current [`VisualMachine`]. 
	///
	/// Return orginal [`Varible`](if any).
	///
	/// Return error when given type is not exist, try to imply a dynamic type or current [`VisualMachine`]'s mutex is poisoned, which shouldn't happen for now.
	pub fn insert_varible(&self, var: impl Into<Varible>) -> Result<Option<Varible>, Error> {
		let var = var.into();
		Ok(self.used_varibles.lock()?.insert(var.name.clone(), var))
	}

	/// Get an exist [`Varible`] into current [`VisualMachine`]. 
	///
	/// Return error when given type is not exist, try to imply a dynamic type or current [`VisualMachine`]'s mutex is poisoned, which shouldn't happen for now.
	pub fn get_varible(&self, var_name: impl Into<String>) -> Result<Option<Varible>, Error> {
		Ok(self.used_varibles.lock()?.get_mut(&var_name.into()).map(|inner| inner.clone()))
	}

	/// remove an exist [`Varible`] into current [`VisualMachine`]. 
	///
	/// Return orginal [`Varible`](if any).
	///
	/// Return error when given type is not exist, try to imply a dynamic type or current [`VisualMachine`]'s mutex is poisoned, which shouldn't happen for now.
	pub fn remove_varible(&self, var_name: impl Into<String>) -> Result<Option<Varible>, Error> {
		Ok(self.used_varibles.lock()?.remove(&var_name.into()))
	}

	fn check_ascii(&self, code: &str) -> Result<(), (String, usize, usize, usize)> {
		for (line_id, line) in code.lines().enumerate() {
			for (col, chara) in line.chars().enumerate() {
				if !chara.is_ascii() {
					return Err((line.to_string(), line_id + 1, col + 1, chara.to_string().len()));
				}
			}
		}
		Ok(())
	}
}

impl VisualMachine {
	fn handle_statement(&mut self, input: &Pair<Rule>) -> Result<Condition, Error> {
		debug!("handling statement");
		let inner = input.clone().into_inner().next();
		if inner.is_none() {
			// println!("awa");
			return Ok(Condition::None);
		}
		let inner = inner.unwrap();
		let out = match inner.as_rule() {
			Rule::Statement => self.handle_statement(&inner),
			// Rule::Auto => self.handle_auto(&inner, &inner.as_str().to_string(), inner.line_col().0),
			Rule::Ask => self.handle_ask(&inner, &inner.as_str().to_string(), inner.line_col().0),
			Rule::Continue => {
				if self.loop_times == 0 {
					Err(Error::new_from_pair(ErrorType::UnexpectContinue, &inner.as_str().to_string(), inner.line_col().0, &inner))
				}else {
					Ok(Condition::Continue)
				}
			},
			Rule::Break => {
				if self.loop_times == 0 {
					Err(Error::new_from_pair(ErrorType::UnexpectBreak, &inner.as_str().to_string(), inner.line_col().0, &inner))
				}else {
					Ok(Condition::Break)
				}
			},
			Rule::Loop => self.handle_loop(&inner),
			Rule::If => self.handle_if(&inner, &inner.as_str().to_string(), inner.line_col().0),
			Rule::Assignment => self.handle_assignment(&inner, &inner.as_str().to_string(), inner.line_col().0),
			Rule::FunctionStatement => {
				self.handle_function(&inner, &inner.as_str().to_string(), inner.line_col().0)?;
				Ok(Condition::None)
			},
			Rule::MacroStatement => self.handle_macro(&inner, &inner.as_str().to_string(), inner.line_col().0),
			Rule::Return => self.handle_return(&inner, &inner.as_str().to_string(), inner.line_col().0),
			Rule::Field => self.handle_field(&inner),
			Rule::NewFunction => self.handle_new_function(&inner, &inner.as_str().to_string(), inner.line_col().0),
			Rule::NewStruct => self.handle_new_object(&inner, &inner.as_str().to_string(), inner.line_col().0),
			Rule::NewMacro => self.handle_new_macro(&inner, &inner.as_str().to_string(), inner.line_col().0),
			Rule::Imply => self.handle_imply(&inner, &inner.as_str().to_string(), inner.line_col().0),
			Rule::Trait => self.handle_interface(&inner, &inner.as_str().to_string(), inner.line_col().0),
			Rule::Comment => Ok(Condition::None),
			Rule::ElementUse => self.handle_element_use(&inner, &inner.as_str().to_string(), inner.line_col().0),
			Rule::VistStructMethodPath => self.handle_visit_method_path(&inner, &inner.as_str().to_string(), inner.line_col().0),
			_ => unreachable!(),
		};
		// println!("{:?}", out);
		out
	}

	fn handle_ask(&mut self, input: &Pair<Rule>, code: &String, start_line: usize) -> Result<Condition, Error> {
		debug!("handling ask statement");
		let mut inner = input.clone().into_inner();
		let element = inner.next().unwrap();
		let var = self.handle_element(&element, code, start_line)?;
		let (ty, value) = (var.ty, var.value);
		let (ty1, ty2);
		match ty {
			VarType::Result(inner1, inner2) => {
				ty1 = inner1;
				ty2 = *inner2;
			},
			_ => return Err(Error::new_from_pair(ErrorType::NotResult(ty), code, start_line, &element)),
		}
		let arm1_var = inner.next().unwrap().as_str().to_string();
		let arm1_code = inner.next().unwrap().into_inner();
		let arm2_var = inner.next().unwrap().as_str().to_string();
		let arm2_code = inner.next().unwrap().into_inner();
		let mut run_code_1 = true;
		let mut out_condition = Condition::None;
		self.field_start();
		match value {
			VarValue::Result(inner) => {
				match inner {
					Ok(value) => {
						let value = *value;
						self.new_var(arm1_var, *ty1, value)?;
					},
					Err(value) => {
						let value = *value;
						run_code_1 = false;
						self.new_var(arm2_var, ty2, value)?;
					},
				}
			},
			_ => unreachable!(),
		}
		if run_code_1 {
			for statement in arm1_code {
				out_condition = out_condition | self.handle_statement(&statement)?;
				match &out_condition {
					Condition::Continue | Condition::Break | Condition::Return => break,
					_ => {}
				}
			}
		}else {
			for statement in arm2_code {
				out_condition = out_condition | self.handle_statement(&statement)?;
				match &out_condition {
					Condition::Continue | Condition::Break | Condition::Return => break,
					_ => {}
				}
			}
		}
		self.field_end()?;
		Ok(out_condition)
	}

	fn handle_loop(&mut self, input: &Pair<Rule>) -> Result<Condition, Error> {
		debug!("handling loop statement");
		let inner = input.clone().into_inner();
		let mut out_condition = Condition::None;
		self.loop_times += 1;
		loop {
			let inner_clone = inner.clone();
			self.field_start();
			let mut need_break = false;
			// println!("total: {}", self.used_varibles.lock()?.values().count())
			for statement in inner_clone {
				let condition = self.handle_statement(&statement)?;
				out_condition = out_condition | condition.clone();
				match out_condition {
					Condition::Continue => break,
					Condition::Break => {
						out_condition = Condition::None;
						need_break = true;
						break;
					},
					Condition::Return => {
						need_break = true;
						break;
					}
					_ => {}
				}
			}
			self.field_end()?;
			if need_break {
				break;
			}
		}
		self.loop_times -= 1;
		Ok(out_condition)
	}

	fn handle_if(&mut self, input: &Pair<Rule>, code: &String, start_line: usize) -> Result<Condition, Error> {
		debug!("handling if statement");
		let inner = input.clone().into_inner();
		let mut temp = false;
		let len = inner.clone().filter_map(|inner| if let Rule::Field = inner.as_rule() {
			Some(0)
		}else {
			None
		}).collect::<Vec<usize>>().len();
		let mut id = 0;
		let mut out_condition = Condition::None;
		let mut meet_if = false;
		for ele in inner {
			match ele.as_rule() {
				Rule::Else => {}, 
				Rule::Caculation => {
					let var = self.handle_element(&ele, code, start_line)?;
					if let VarType::Bool = var.ty {
						temp = if let VarValue::Bool(inner) = var.value {
							meet_if = true;
							inner
						}else {
							unreachable!()
						};
					}else {
						return Err(Error::new_from_pair(ErrorType::ExpectFound(VarType::Bool, var.ty), code, start_line, &ele));
					}
					if !var.name.is_empty() {
						self.used_varibles.lock()?.insert(var.name.to_string(), var);
					}
				},
				Rule::Field => {
					// println!("{:?}", temp);
					if id + 1 != len || meet_if {
						if temp {
							out_condition = out_condition | self.handle_field(&ele)?;
						}
					}else {
						out_condition = out_condition | self.handle_field(&ele)?;
					}
					id += 1;
					meet_if = false;
				},
				_ => unreachable!(),
			};
			match out_condition {
				Condition::None => {},
				_ => break,
			}
		}
		Ok(out_condition)
	}

	fn handle_assignment(&mut self, input: &Pair<Rule>, code: &String, start_line: usize) -> Result<Condition, Error> {
		debug!("handling assignment statement");
		// println!("{:?}", code);
		let mut inner = input.clone().into_inner();
		let var_pair = inner.next().unwrap();
		let var_name = var_pair.as_str().to_string();
		let caculation = inner.next().unwrap();
		let varible = self.handle_element(&caculation, code, start_line)?;
		let (ty, value) = (varible.ty, varible.value);
		let mut used_varibles = self.used_varibles.lock()?;
		if let Some(var) = used_varibles.get_mut(&var_name) {
			if !self.check_type_is_vaild(&var.ty, &ty, &caculation, true)? {
				return Err(Error::new_from_pair(ErrorType::ExpectFound(var.ty.clone(), ty), code, start_line, &var_pair));
			}else {
				var.value = value;
				if let VarType::None = var.ty {
					var.ty = ty.clone();
				}
				if let VarValue::None = var.value {
					var.ty = VarType::None;
				}
				if let VarType::Dynamic(_) = var.ty {
					var.ty = ty;
				}
				return Ok(Condition::None);
			}
		}
		drop(used_varibles);
		self.new_var(var_name, ty, value)?;
		Ok(Condition::None)
	}

	fn handle_function(&mut self, input: &Pair<Rule>, code: &String, start_line: usize) -> Result<Varible, Error> {
		debug!("handling function statement");
		let mut inner = input.clone().into_inner();
		let mut inner = inner.next().unwrap().into_inner();
		let function_pair = inner.next().unwrap();
		let function_name = function_pair.as_str().to_string();
		let mut input_varible_vec = vec!();
		for caculation in inner {
			input_varible_vec.push(self.handle_element(&caculation, code, start_line)?);
		}
		let mut regstrated_functions = self.regstrated_functions.lock()?;
		let mut input_varible_map = HashMap::new();
		input_varible_map.insert("return".to_string(), Varible {
			name: "return".to_string(),
			ty: VarType::None,
			value: VarValue::None,
			nesting_times: 0,
		});
		let mut var_need_change = HashMap::new();
		let (func_code, output_varible) = if let Some(function) = regstrated_functions.get_mut(&function_name) {
			let input_total = input_varible_vec.len();
			if function.input_varible.len() != input_total {
				return Err(Error::new_from_pair(ErrorType::VaribleAmountUnmatch(function.input_varible.len(), input_total), code, start_line, &function_pair));
			}else {
				for (caculated_var, (name, ty)) in input_varible_vec.into_iter().zip(function.input_varible.iter()) {
					if !self.check_type_is_vaild(ty, &caculated_var.ty, input, false)? {
						return Err(Error::new_from_pair(ErrorType::ExpectFound(ty.clone(), caculated_var.ty), code, start_line, &function_pair));
					}
					if !caculated_var.name.is_empty() && !name.is_empty() {
						// println!("{}, {}", name.clone(), caculated_var.name.clone());
						var_need_change.insert(name.clone(), caculated_var.name.clone());
					}
					input_varible_map.insert(name.clone(), Varible {
						name: name.clone(),
						ty: caculated_var.ty,
						value: caculated_var.value,
						nesting_times: self.nesting_times,
					});
				}
				match &mut function.code {
					Code::Code(func_code) => {
						debug!("running function code");
						(func_code.clone(), function.output_varible.clone())
					},
					Code::RustFunction(inner) => {
						debug!("running rust function code");
						match (inner)(&mut input_varible_map) {
							Ok(out) => {
								if !self.check_type_is_vaild(&function.output_varible, &out.ty, &function_pair, false)?  {
									return Err(Error::new_from_pair(ErrorType::ExpectFound(function.output_varible.clone(), out.ty), code, start_line, &function_pair));
								}
								for (key, name) in var_need_change {
									if let Some(var) = input_varible_map.get(&key) {
										self.used_varibles.lock()?.insert(name, var.clone());
									}
								}
								// println!("{:?}", self.used_varibles.lock()?.get("b"));
								return Ok(Varible {
									name: "return".into(),
									nesting_times: 0,
									..out
								});
							},
							Err(e) => {
								return Err(Error::new_from_pair(e.into(), code, start_line, &function_pair))
							}
						};
					}
				}
			}
		}else {
			return Err(Error::new_from_pair(ErrorType::FunctionNotFound(function_name), code, start_line, &function_pair));
		};
		drop(regstrated_functions);
		let mut sub_vm = self.clone();
		sub_vm.used_varibles = Arc::new(Mutex::new(input_varible_map));
		sub_vm.return_value_type = Some(output_varible.clone());
		sub_vm.current_function_name = Some(function_name.clone());
		sub_vm.run_script(&func_code)?;
		let mut used_varibles = sub_vm.used_varibles.lock()?;
		for (key, name) in var_need_change {
			if let Some(var) = used_varibles.get(&key) {
				self.used_varibles.lock()?.insert(name, var.clone());
			}
		} 
		if let Some(VarType::None) = sub_vm.return_value_type {
			Ok(Varible {
				name: "return".to_string(),
				ty: VarType::None,
				value: VarValue::None,
				nesting_times: 0,
			})
		}else if let Some(var) = used_varibles.remove(&"return".to_string()) {
			if !self.check_type_is_vaild(&output_varible, &var.ty, &function_pair, false)?  {
				return Err(Error::new_from_pair(ErrorType::ExpectFound(output_varible.clone(), var.ty), code, start_line, &function_pair));
			}
			Ok(var)
		}else {
			unreachable!()
		}
	}

	fn handle_method(&mut self, input: &Pair<Rule>, code: &String, start_line: usize, input_ty: &VarType) -> Result<Varible, Error> {
		// println!("{:?}", code);
		debug!("handling function statement");
		// println!("{:#?}", input);
		// let inner = input.clone().into_inner().next().unwrap();

		let input_ty = if let VarType::Array(_) = input_ty {
			&VarType::ArrayAny
		}else {
			input_ty
		};
		self.current_self_type = Some(input_ty.clone());
		let mut inner = input.clone().into_inner();
		// let mut inner = inner.next().unwrap().into_inner();
		// let mut inner = inner.next().unwrap().into_inner();
		let function_pair = inner.next().unwrap();
		let function_name = function_pair.as_str().to_string();
		// rust force me to do this
		let mut input_varible_vec = vec!();
		let mut input_varible_vec_2 = vec!();
		for caculation in inner.clone() {
			input_varible_vec.push(self.handle_element(&caculation, code, start_line)?);
		}
		for caculation in inner {
			input_varible_vec_2.push(self.handle_element(&caculation, code, start_line)?);
		}
		let mut input_varible_map = HashMap::new();
		let mut var_need_change = HashMap::new();
		let (func_code, output_varible) = {
			let (mut func_code, mut output_varible) = (None, None);
			let mut implied_interfaces = self.implied_interfaces.lock()?;
			if let Some((regstrated_method, regstrated_interfaces)) = implied_interfaces.get_mut(input_ty) {
				// TODO: change this block to a closure
				for interface in regstrated_interfaces.values_mut() {
					if let Some(function) = interface.function.get_mut(&function_name) {
						let input_total = input_varible_vec.len();
						if function.input_varible.len() != input_total {
							return Err(Error::new_from_pair(ErrorType::VaribleAmountUnmatch(function.input_varible.len(), input_total), code, start_line, &function_pair));
						}else {
							for (caculated_var, (name, ty)) in input_varible_vec.into_iter().zip(function.input_varible.iter()) {
								if !self.check_type_is_vaild(ty, &caculated_var.ty, input, false)? {
									return Err(Error::new_from_pair(ErrorType::ExpectFound(ty.clone(), caculated_var.ty), code, start_line, &function_pair));
								}
								if !caculated_var.name.is_empty() {
									var_need_change.insert(name.clone(), caculated_var.name.clone());
								}
								input_varible_map.insert(name.clone(), Varible {
									name: name.clone(),
									ty: caculated_var.ty,
									value: caculated_var.value,
									nesting_times: caculated_var.nesting_times,
								});
							}
							match &mut function.code {
								Code::Code(func_code_innner) => {
									debug!("running function code");
									func_code = Some(func_code_innner.clone());
									output_varible = Some(function.output_varible.clone());
								},
								Code::RustFunction(inner) => {
									debug!("running rust function code");
									match (inner)(&mut input_varible_map) {
										Ok(out) => {
											if !self.check_type_is_vaild(&function.output_varible, &out.ty, &function_pair, false)?  {
												return Err(Error::new_from_pair(ErrorType::ExpectFound(function.output_varible.clone(), out.ty), code, start_line, &function_pair));
											}
											for (key, name) in var_need_change {
												// println!("{:?}", name);
												if let Some(var) = input_varible_map.get(&key) {
													self.used_varibles.lock()?.insert(name, var.clone());
												}
											}
											return Ok(Varible {
												name: "return".into(),
												nesting_times: 0,
												..out
											});
										},
										Err(e) => {
											return Err(Error::new_from_pair(e.into(), code, start_line, &function_pair))
										}
									};
								}
							}
							break;
						}
					};
				}
				if func_code.is_none() {
					if let Some(function) = regstrated_method.get_mut(&function_name) {
						let input_total = input_varible_vec_2.len();
						if function.input_varible.len() != input_total {
							return Err(Error::new_from_pair(ErrorType::VaribleAmountUnmatch(function.input_varible.len(), input_total), code, start_line, &function_pair));
						}else {
							for (caculated_var, (name, ty)) in input_varible_vec_2.into_iter().zip(function.input_varible.iter()) {
								if !self.check_type_is_vaild(ty, &caculated_var.ty, input, false)? {
									return Err(Error::new_from_pair(ErrorType::ExpectFound(ty.clone(), caculated_var.ty), code, start_line, &function_pair));
								}
								if !caculated_var.name.is_empty() {
									var_need_change.insert(name.clone(), caculated_var.name.clone());
								}
								input_varible_map.insert(name.clone(), Varible {
									name: name.clone(),
									ty: caculated_var.ty,
									value: caculated_var.value,
									nesting_times: caculated_var.nesting_times,
								});
							}
							match &mut function.code {
								Code::Code(func_code_innner) => {
									debug!("running function code");
									func_code = Some(func_code_innner.clone());
									output_varible = Some(function.output_varible.clone());
								},
								Code::RustFunction(inner) => {
									debug!("running rust function code");
									match (inner)(&mut input_varible_map) {
										Ok(out) => {
											if !self.check_type_is_vaild(&function.output_varible, &out.ty, &function_pair, false)?  {
												return Err(Error::new_from_pair(ErrorType::ExpectFound(function.output_varible.clone(), out.ty), code, start_line, &function_pair));
											}
											for (key, name) in var_need_change {
												// println!("{:?}", name);
												if let Some(var) = input_varible_map.get(&key) {
													self.used_varibles.lock()?.insert(name, var.clone());
												}
											} 
											return Ok(Varible {
												name: "return".into(),
												nesting_times: 0,
												..out
											});
										},
										Err(e) => {
											return Err(Error::new_from_pair(e.into(), code, start_line, &function_pair))
										}
									};
								}
							}
						}
					};
				}
				if let (Some(func_code), Some(out)) = (func_code, output_varible) {
					(func_code, out)
				}else {
					return Err(Error::new_from_pair(ErrorType::MethodNotFound(function_name), code, start_line, &function_pair));
				}
			}else {
				return Err(Error::new_from_pair(ErrorType::TypeNotFound(input_ty.name()), code, start_line, input));
			}
		};
		
		let mut sub_vm = self.clone();
		input_varible_map.insert("return".to_string(), Varible {
			name: "return".to_string(),
			ty: VarType::None,
			value: VarValue::None,
			nesting_times: 0,
		});
		sub_vm.used_varibles =  Arc::new(Mutex::new(input_varible_map));
		sub_vm.current_self_type = Some(input_ty.clone());
		sub_vm.return_value_type = Some(output_varible);
		sub_vm.current_function_name = Some(function_name.clone());
		sub_vm.run_script(&func_code)?;
		self.current_self_type = None;
		let mut used_varibles = sub_vm.used_varibles.lock()?;
		for (key, name) in var_need_change {
			if let Some(var) = used_varibles.get(&key) {
				self.used_varibles.lock()?.insert(name, var.clone());
			}
		} 
		if let Some(VarType::None) = sub_vm.return_value_type {
			Ok(Varible {
				name: "return".to_string(),
				ty: VarType::None,
				value: VarValue::None,
				nesting_times: 0,
			})
		}else if let Some(var) = used_varibles.remove(&"return".to_string()) {
			Ok(var)
		}else {
			unreachable!()
		}
	}

	fn handle_macro(&mut self, input: &Pair<Rule>, code: &String, start_line: usize) -> Result<Condition, Error> {
		debug!("handling macro statement `{}`", code);
		let inner = input.clone().into_inner().next().unwrap();
		let mut inner = inner.into_inner();
		let macro_pair = inner.next().unwrap();
		let macro_name = macro_pair.as_str().to_string();
		let mut input_var = vec!();
		// rust force me to do this
		let mut input_var_2 = vec!();
		for string in inner {
			input_var.push(VarValue::String(string.as_str().to_string()));
			input_var_2.push(VarValue::String(string.as_str().to_string()));
		}
		let regstrated_macros = self.regstrated_macros.lock()?;
		let mut is_rust_function = false;
		let (input_name, macro_code) = if let Some(inner) = regstrated_macros.get(&macro_name) {
			match &inner.code {
				Code::Code(func) => {
					(inner.input_name.clone(), func.clone())
				},
				Code::RustFunction(func) => {
					is_rust_function = true;
					let back = match (func)(&mut HashMap::from([(inner.input_name.clone(), Varible {
						name: inner.input_name.clone(),
						ty: VarType::Array(Box::new(VarType::String)),
						value: VarValue::Array(input_var_2),
						nesting_times: self.nesting_times + 1,
					})])) {
						Ok(t) => t,
						Err(e) => return Err(Error::new_from_pair(ErrorType::from(e), code, start_line, &macro_pair)),
					};
					if let VarValue::String(out) = back.value {
						(inner.input_name.clone(), out)
					}else {
						return Err(Error::new_from_pair(ErrorType::ExpectFound(VarType::String, back.ty), code, start_line, &macro_pair));
					}
				}
			}
		}else {
			return Err(Error::new_from_pair(ErrorType::MacroNotFound(macro_name), code, start_line, &macro_pair));
		};
		drop(regstrated_macros);
		let macro_code = if is_rust_function {
			macro_code
		}else {
			let mut sub_vm = self.clone();
			let mut input_varible_map = HashMap::new();
			input_varible_map.insert("return".to_string(), Varible {
				name: "return".to_string(),
				ty: VarType::None,
				value: VarValue::None,
				nesting_times: 0,
			});
			input_varible_map.insert(input_name.clone(), Varible {
				name: input_name.clone(),
				ty: VarType::Array(Box::new(VarType::String)),
				value: VarValue::Array(input_var),
				nesting_times: self.nesting_times + 1,
			});
			sub_vm.used_varibles = Arc::new(Mutex::new(input_varible_map));
			sub_vm.return_value_type = Some(VarType::String);
			sub_vm.current_function_name = Some(macro_name.clone());
			sub_vm.run_script(&macro_code)?;
			let mut used_varibles = sub_vm.used_varibles.lock()?;
			if let Some(var) = used_varibles.remove(&"return".to_string()) {
				if let VarValue::String(inner) = var.value {
					inner
				}else {
					return Err(Error::new_from_pair(ErrorType::ExpectFound(var.ty.clone(), VarType::String), code, start_line, &macro_pair));
				}
			}else {
				unreachable!()
			}
		};
		self.current_function_name = Some("complier function".to_string());
		self.run_script(&macro_code)?;
		self.current_function_name = None;
		Ok(Condition::None)
	}

	fn handle_return(&mut self, input: &Pair<Rule>, code: &String, start_line: usize) -> Result<Condition, Error> {
		debug!("handling return statement");
		if let Some(require_ty) = self.return_value_type.clone() {
			let inner = input.clone().into_inner().next();
			if let Some(pair) = inner {
				let re = self.handle_element(&pair, code, start_line)?;
				let (ty, value) = (re.ty, re.value);
				if !self.check_type_is_vaild(&require_ty, &ty, &pair, false)? {
					Err(Error::new_from_pair(ErrorType::ExpectFound(require_ty, ty), code, start_line, &pair))
				}else {
					self.new_var("return".to_string(), ty, value)?;
					if let Some(re) = self.used_varibles.lock()?.get_mut(&"return".to_string()) {
						re.nesting_times = 0;
					}
					Ok(Condition::Return)
				}
			}else if let VarType::None = require_ty {
				self.new_var("return".to_string(), VarType::None, VarValue::None)?;
				if let Some(re) = self.used_varibles.lock()?.get_mut(&"return".to_string()) {
					re.nesting_times = 0;
				}
				Ok(Condition::Return)
			}else {
				Err(Error::new_from_pair(ErrorType::ExpectFound(VarType::None, require_ty.clone()), code, start_line, input))
			}
		}else {
			Err(Error::new_from_pair(ErrorType::UnexpectReturn, code, start_line, input))
		}
	}

	fn handle_field(&mut self, input: &Pair<Rule>) -> Result<Condition, Error> {
		debug!("handling feild statement");
		let inner = input.clone().into_inner();
		let mut out_condition = Condition::None;
		self.field_start();
		for statement in inner {
			out_condition = out_condition | self.handle_statement(&statement)?;
			match out_condition {
				Condition::None => {},
				_ => break,
			}
			// println!("{:?}", out_condition);
		}
		self.field_end()?;
		// println!("{:?}", out_condition);
		Ok(out_condition)
	}

	fn handle_new_function(&mut self, input: &Pair<Rule>, code_input: &String, start_line: usize) -> Result<Condition, Error> {
		debug!("handling new function statement");
		let func = self.handle_function_inner(input, code_input, start_line, None)?;
		let mut regstrated_functions = self.regstrated_functions.lock()?;
		regstrated_functions.insert(func.name.clone(), func);
		Ok(Condition::None)
	}

	fn handle_new_object(&mut self, input: &Pair<Rule>, code_input: &String, start_line: usize) -> Result<Condition, Error> {
		debug!("handling new object statement");
		let mut inner = input.clone().into_inner();
		let object_pair = inner.next().unwrap();
		let object_name = object_pair.as_str().to_string();
		if self.regstrated_objects.lock()?.get(&object_name).is_some() {
			return Err(Error::new_from_pair(ErrorType::ObjectExist(object_name), code_input, start_line, &object_pair));
		}
		let mut var_names = vec!();
		let mut var_types = vec!();
		let mut field = HashMap::new();
		for element in inner {
			match element.as_rule() {
				Rule::VarName => {
					let name = element.as_str().to_string();
					if var_names.contains(&name) {
						return Err(Error::new_from_pair(ErrorType::FieldExist(name), code_input, start_line, &element));
					}
					var_names.push(name);
				},
				Rule::Type => {
					var_types.push(self.handle_type(&element, code_input, start_line)?);
				},
				_ => unreachable!(),
			}
		}
		for (var_name, var_type) in var_names.into_iter().zip(var_types.into_iter()) {
			field.insert(var_name, var_type);
		}
		self.implied_interfaces.lock()?.insert(VarType::Object(object_name.clone()), (HashMap::new(), HashMap::from([("Any".to_string(), Interface::any())])));
		self.regstrated_objects.lock()?.insert(object_name.clone(), Object::CustomObject(CustomObject {
			name: object_name,
			fields: field,
		}));
		Ok(Condition::None)
	}

	fn handle_new_macro(&mut self, input: &Pair<Rule>, code_input: &String, start_line: usize) -> Result<Condition, Error> {
		debug!("handling new macro statement");
		let mut inner = input.clone().into_inner();
		let macro_pair = inner.next().unwrap();
		let macro_name = macro_pair.as_str().to_string();
		if self.regstrated_macros.lock()?.get(&macro_name).is_some() {
			return Err(Error::new_from_pair(ErrorType::ObjectExist(macro_name), code_input, start_line, &macro_pair));
		}
		let input_name = inner.next().unwrap().as_str().to_string();
		let code = inner.next().unwrap().as_str().to_string();
		self.regstrated_macros.lock()?.insert(macro_name.clone(), Macro {
			code: Code::Code(code),
			input_name,
			name: macro_name,
		});
		Ok(Condition::None)
	}

	fn handle_imply(&mut self, input: &Pair<Rule>, code_input: &String, start_line: usize) -> Result<Condition, Error> {
		debug!("handling imply macro statement");
		let inner = input.clone().into_inner();
		let mut interface_name = String::new();
		let mut type_to_imply = VarType::None;
		let mut regstrated_method = HashMap::new();
		for statement in inner {
			match statement.as_rule() {
				Rule::VarName => {
					interface_name = statement.as_str().to_string();
				},
				Rule::Type => {
					type_to_imply = self.handle_type(&statement, code_input, start_line)?;
					if type_to_imply == VarType::None { return Err(Error::new_from_pair(ErrorType::CantImplyNone, code_input, start_line, &statement)) };
					self.current_self_type = Some(type_to_imply.clone());
				},
				Rule::NewFunction => {
					let function = self.handle_function_inner(&statement, &statement.as_str().to_string(), statement.line_col().0, Some(&regstrated_method))?;
					if !interface_name.is_empty() {
						let regstrated_interfaces = self.regstrated_interfaces.lock()?;
						if let Some(interface) = regstrated_interfaces.get(&interface_name) {
							if !interface.function.contains_key(&function.name) {
								return Err(Error::new_from_pair(ErrorType::SuperfluousFunction(interface_name, function.name.clone()), code_input, start_line, &statement)); 
							}else {
								regstrated_method.insert(function.name.clone(), function);
							}
						}else {
							return Err(Error::new_from_pair(ErrorType::InterfaceNotFound(interface_name), code_input, start_line, input)); 
						}
					}else {
						regstrated_method.insert(function.name.clone(), function);
					}
				},
				_ => unreachable!(),
			}
		}
		if type_to_imply.is_dynamic() || type_to_imply.is_array() || type_to_imply.is_result() {
			return Err(Error::new_from_pair(ErrorType::UnsupportImply, code_input, start_line, input)); 
		}
		if !interface_name.is_empty() {
			let regstrated_interfaces = self.regstrated_interfaces.lock()?;
			if let Some(interface) = regstrated_interfaces.get(&interface_name) {
				let mut required_keys: HashSet<String> = interface.function.iter().filter_map(|(key, val)| {
					if let Code::Code(_) = &val.code {
						Some(key.clone())
					}else {
						None
					}
				}).collect();
				for key in regstrated_method.keys() {
					required_keys.remove(key);
				}
				if !required_keys.is_empty() {
					let mut missing_function = String::new();
					for key in required_keys {
						missing_function = format!("{missing_function}{key}, ");
					}
					return Err(Error::new_from_pair(ErrorType::MissingFunction(interface_name, missing_function), code_input, start_line, input)); 
				}
			}else {
				return Err(Error::new_from_pair(ErrorType::InterfaceNotFound(interface_name), code_input, start_line, input)); 
			}
		}
		let mut implied_interfaces = self.implied_interfaces.lock()?;
		if let Some(object) = implied_interfaces.get_mut(&type_to_imply) {
			if interface_name.is_empty() {
				for (key, function) in regstrated_method {
					let function_name = function.name.clone();
					if object.0.contains_key(&key) {
						return Err(Error::new_from_pair(ErrorType::MethodExist(function_name), code_input, start_line, input)); 
					}else {
						object.0.insert(key.clone(), function);
					}
				}
			}else {
				for interface in object.1.values() {
					for name in interface.function.keys() {
						if regstrated_method.contains_key(name) {
							return Err(Error::new_from_pair(ErrorType::MethodExist(name.clone()), code_input, start_line, input));
						}
					}
				}
				object.1.insert(interface_name.clone(), Interface {
					name: interface_name,
					function: regstrated_method,
				});
			};
		}else {
			return Err(Error::new_from_pair(ErrorType::TypeNotFound(type_to_imply.name()), code_input, start_line, input)); 
		}
		self.current_self_type = None;
		Ok(Condition::None)
	}

	fn handle_function_inner(&mut self, input: &Pair<Rule>, code_input: &String, start_line: usize, is_imply: Option<&HashMap<String, Function>>) -> Result<Function, Error> {
		debug!("generationg function statement");
		let mut inner = input.clone().into_inner();
		let function_pair = inner.next().unwrap();
		let function_name = function_pair.as_str().to_string();
		let mut var_names = vec!();
		let mut var_types = vec!();
		let mut code = String::new();
		let mut input_varible = vec!();
		let output_varible;
		if let Some(map) = is_imply {
			if map.get(&function_name).is_some() {
				return Err(Error::new_from_pair(ErrorType::MethodExist(function_name), code_input, start_line, &function_pair));
			}
		}else if self.regstrated_functions.lock()?.get(&function_name).is_some() {
			return Err(Error::new_from_pair(ErrorType::FunctionExist(function_name), code_input, start_line, &function_pair));
		}
		
		for element in inner {
			match element.as_rule() {
				Rule::SelfIdent => {
					if let Some(self_type) = &self.current_self_type {
						var_names.push("self".to_string());
						var_types.push(self_type.clone());
					}else {
						return Err(Error::new_from_pair(ErrorType::UnexpectSelf, code_input, start_line, &element));
					}
				},
				Rule::VarName => {
					let name = element.as_str().to_string();
					if var_names.contains(&name) {
						return Err(Error::new_from_pair(ErrorType::VarExist(name), code_input, start_line, &element));
					}
					var_names.push(name);
				},
				Rule::Type => {
					var_types.push(self.handle_type(&element, code_input, start_line)?);
				},
				Rule::Field => {
					code = format!("{code}{}", element.as_str());
				},
				_ => unreachable!()
			}
		}
		if var_names.len() == var_types.len() {
			for zipped in var_names.into_iter().zip(var_types.into_iter()) {
				input_varible.push(zipped)
			}
			output_varible = VarType::None;
		}else {
			output_varible = var_types.remove(var_types.len() - 1);
			for zipped in var_names.into_iter().zip(var_types.into_iter()) {
				input_varible.push(zipped)
			}
		}
		Ok(Function {
			output_varible,
			input_varible,
			name: function_name,
			code: Code::Code(code),
			nesting_times: self.nesting_times
		})
	}

	fn handle_interface(&mut self, input: &Pair<Rule>, code_input: &String, start_line: usize) -> Result<Condition, Error> {
		debug!("handling interface statement");
		let mut inner = input.clone().into_inner();
		let interface_pair = inner.next().unwrap();
		let interface_name = interface_pair.as_str().to_string();
		if self.regstrated_interfaces.lock()?.get(&interface_name).is_some() {
			return Err(Error::new_from_pair(ErrorType::InterfaceExist(interface_name), code_input, start_line, &interface_pair));
		}
		let mut function = HashMap::new();
		self.current_self_type = Some(VarType::Dynamic(interface_name.clone()));
		for method in inner {
			let mut function_new = self.handle_function_inner(&method, &method.as_str().to_string(), method.line_col().0, Some(&function))?;
			if let Code::Code(code) = &mut function_new.code {
				code.clear();
			}
			function.insert(function_new.name.clone(), function_new);
		}
		self.implied_interfaces.lock()?.insert(VarType::Dynamic(interface_name.clone()), (HashMap::new(), HashMap::new()));
		self.regstrated_interfaces.lock()?.insert(interface_name.clone(), Interface {
			name: interface_name,
			function,
		});
		self.current_self_type = None;
		Ok(Condition::None)
	}

	fn handle_type(&mut self, input: &Pair<Rule>, code: &String, start_line: usize) -> Result<VarType, Error> {
		debug!("parsing type");
		let inner = input.clone().into_inner().next().unwrap();
		match inner.as_rule() {
			Rule::Num => Ok(VarType::Number),
			Rule::Bool => Ok(VarType::Bool),
			Rule::String => Ok(VarType::String),
			Rule::SelfIdentType => {
				if let Some(self_type) = &self.current_self_type {
					Ok(self_type.clone())
				}else {
					Err(Error::new_from_pair(ErrorType::UnexpectSelf, code, start_line, &inner))
				}
			},
			Rule::Array => {
				let mut inner_value = inner.clone().into_inner();
				inner_value.next();
				let ty = inner_value.next().unwrap();
				Ok(VarType::Array(Box::new(self.handle_type(&ty, code, start_line)?)))
			},
			Rule::Result => {
				let mut inner_value = inner.clone().into_inner();
				inner_value.next();
				let ty1 = inner_value.next().unwrap();
				let ty2 = inner_value.next().unwrap();
				Ok(VarType::Result(Box::new(self.handle_type(&ty1, code, start_line)?), Box::new(self.handle_type(&ty2, code, start_line)?)))
			},
			Rule::Dynamic => {
				let mut inner_value = inner.clone().into_inner();
				inner_value.next();
				let inner_iter = inner_value.next().unwrap();
				let inner = inner_iter.as_str().to_string();
				let regstrated_interfaces = self.regstrated_interfaces.lock()?;
				if AUTO_INTERFACE.contains(&inner.as_str()) {
					return Ok(VarType::Dynamic(inner));
				}
				if regstrated_interfaces.get(&inner).is_some() {
					Ok(VarType::Dynamic(inner))
				}else {
					Err(Error::new_from_pair(ErrorType::InterfaceNotFound(inner), code, start_line, &inner_iter))
				}
			},
			Rule::VarName => {
				let object_name = inner.as_str().to_string();
				let regstrated_objects = self.regstrated_objects.lock()?;
				if regstrated_objects.get(&object_name).is_some() {
					Ok(VarType::Object(object_name))
				}else {
					Err(Error::new_from_pair(ErrorType::TypeNotFound(object_name), code, start_line, &inner))
				}
			},
			_ => unreachable!(),
		}
	}

	// Element, Parentheses and Caculation
	fn handle_element(&mut self, input: &Pair<Rule>, code: &String, start_line: usize)  -> Result<Varible, Error> {
		debug!("handling element");
		let caculation_tree = self.handle_caculation(input, code, start_line)?;
		debug!("caculating result");
		self.caculate(caculation_tree, code, start_line, input)
	}

	fn handle_caculation(&mut self, input: &Pair<Rule>, code: &String, start_line: usize) -> Result<Caculation, Error> {
		debug!("gnerating caculation tree");
		// println!("{}", code);
		match input.as_rule() {
			Rule::Element => {
				let mut inner_iter = input.clone().into_inner();
				let inner = inner_iter.next().unwrap();
				let out = match inner.as_rule() {
					Rule::None => {
						Caculation {
							tree: CaculationTree::TempValue(VarType::None, VarValue::None), 
							..Default::default()
						}
					}
					Rule::Parentheses => self.handle_caculation(&inner, code, start_line)?,
					Rule::VistStructMethodPath => {
						let mut inner = inner.into_inner();
						let ty = inner.next().unwrap();
						let ty  = self.handle_type(&ty, code, start_line)?;
						let total_code = inner.next().unwrap().as_str().to_string();
						Caculation {
							tree: CaculationTree::MethodUse {
								ty,
								total_code,
							}, 
							..Default::default()
						}
					},
					Rule::Function => {
						Caculation {
							tree: CaculationTree::FunctionUse {
								total_code: inner.as_str().to_string(),
							}, 
							..Default::default()
						}
					},
					Rule::StructAssign => {
						let (obj_type, obj) = self.handle_struct_assgin(&inner, code, start_line)?;
						Caculation {
							tree: CaculationTree::TempValue(obj_type, obj), 
							..Default::default()
						}
					},
					Rule::NotOp => {
						let mut inner = inner.into_inner();
						inner.next();
						Caculation {
							tree: CaculationTree::PreOp {
								is_neg: false,
								rhs: Box::new(self.handle_caculation(&inner.next().unwrap(), code, start_line)?),
							}, 
							..Default::default()
						}
					},
					Rule::NegOp => {
						let mut inner = inner.into_inner();
						inner.next();
						Caculation {
							tree: CaculationTree::PreOp {
								is_neg: true,
								rhs: Box::new(self.handle_caculation(&inner.next().unwrap(), code, start_line)?),
							}, 
							..Default::default()
						}
					},
					Rule::GetValue => {
						let mut inner = inner.into_inner();
						let var_name = inner.next().unwrap().as_str().to_string();
						Caculation {
							tree: CaculationTree::Value(var_name),
							get_value: Some(Box::new(self.handle_caculation(&inner.next().unwrap(), code, start_line)?)), 
							..Default::default()
						}
					},
					Rule::True => {
						Caculation {
							tree: CaculationTree::TempValue(VarType::Bool, VarValue::Bool(true)), 
							..Default::default()
						}
					},
					Rule::False => {
						Caculation {
							tree: CaculationTree::TempValue(VarType::Bool, VarValue::Bool(false)), 
							..Default::default()
						}
					},
					Rule::VarName => {
						let var_name = inner.as_str().to_string();
						Caculation {
							tree: CaculationTree::Value(var_name), 
							..Default::default()
						}
					},
					Rule::Number => {
						let number_string = input.as_str().trim().to_string();
						let num: f64 = number_string.parse().unwrap_or(f64::NAN);
						Caculation {
							tree: CaculationTree::TempValue(VarType::Number, VarValue::Number(num)),
							..Default::default()
						}
					},
					Rule::StringMatch => {
						// let mut inner = inner.into_inner();
						let var = inner.as_str().to_string();
						let var = var[1..var.len() - 1].to_string().replace("\\\\", "\\");
						let var = var.to_string().replace("\\\"", "\"");
						Caculation {
							tree: CaculationTree::TempValue(VarType::String, VarValue::String(var)),
							..Default::default()
						}
					},
					Rule::ArrayDeclare => {
						let inner = inner.into_inner();
						let mut vars = vec!();
						let mut ty = VarType::None;
						for element in inner {
							let var = self.handle_element(&element, code, start_line)?;
							if var.ty.is_result() {
								return Err(Error::new_from_pair(ErrorType::Custom("cant insert a `Result` value into array".to_string(), "remove the element or check your spell".to_string()), code, start_line, &element));
							}
							if vars.is_empty() {
								ty = var.ty.clone();
								vars.push(var.value);
							}else if ty == var.ty {
								vars.push(var.value);
							}else {
								return Err(Error::new_from_pair(ErrorType::ExpectFound(ty, var.ty), code, start_line, &element));
							}
						}
						Caculation {
							tree: CaculationTree::TempValue(VarType::Array(Box::new(ty)), VarValue::Array(vars)),
							..Default::default()
						}
					}
					Rule::Pass => {
						let mut inner = inner.into_inner();
						let var = inner.next().unwrap();
						let var = self.handle_element(&var, code, start_line)?;
						Caculation {
							tree: CaculationTree::TempValue(
								VarType::Result(Box::new(var.ty), Box::new(VarType::Dynamic("Any".to_string()))), 
								VarValue::Result(Ok(Box::new(var.value))),
							),
							..Default::default()
						}
					},
					Rule::Block => {
						let mut inner = inner.into_inner();
						let var = inner.next().unwrap();
						let var = self.handle_element(&var, code, start_line)?;
						Caculation {
							tree: CaculationTree::TempValue(
								VarType::Result(Box::new(VarType::Dynamic("Any".to_string())), Box::new(var.ty)), 
								VarValue::Result(Err(Box::new(var.value))),
							),
							..Default::default()
						}
					},
					_ => unreachable!()
				};
				let inner = inner_iter.next();
				if let Some(visit) = inner {
					let mut visit = visit.into_inner();
					visit.next();
					let visit = visit.next().unwrap();
					match visit.as_rule() {
						Rule::Function => {
							let mut function_inner = visit.into_inner();
							let function_name = function_inner.next().unwrap().as_str().to_string();
							let mut caculations = vec!();
							for caculate in function_inner {
								caculations.push(self.handle_caculation(&caculate, code, start_line)?);
							}
							Ok(Caculation {
								visit_function: Some((function_name,caculations)
									),
								..out
							})
						},
						Rule::VarName => {
							Ok(Caculation {
								visit_field: Some(visit.as_str().to_string()),
								..out
							})
						},
						_ => unreachable!()
					}
				}else {
					Ok(out)
				}
			},
			Rule::Parentheses => {
				let mut inner = input.clone().into_inner();
				if let Some(caculation) = inner.next() {
					self.handle_caculation(&caculation, code, start_line)
				}else {
					Ok(Caculation {
						tree: CaculationTree::TempValue(VarType::None, VarValue::None),
						..Default::default()
					})
				}
			},
			Rule::Caculation => {
				let caculation_inner = input.clone().into_inner();
				PRATT_PARSER.map_primary(|primary| {
					self.handle_caculation(&primary, code, start_line)
				}).map_infix(|lhs, op, rhs| {
					let op = match op.as_rule() {
						Rule::Add => Operation::Add,
						Rule::Minus => Operation::Minus,
						Rule::Cross => Operation::Cross,
						Rule::Divid => Operation::Divid,
						Rule::Power => Operation::Power,
						Rule::LargerOrEqualThan => Operation::LargerOrEqualThan,
						Rule::LessOrEqualThan => Operation::LessOrEqualThan,
						Rule::LargeThan => Operation::LargeThan,
						Rule::LessThan => Operation::LessThan,
						Rule::Unequal => Operation::Unequal,
						Rule::Same => Operation::Same,
						Rule::And => Operation::And,
						Rule::Or => Operation::Or,
						_ => unreachable!(),
					};
					Ok(Caculation {
						tree: CaculationTree::Op {
							lhs: Box::new(lhs?),
							op,
							rhs: Box::new(rhs?),
						}, 
						..Default::default()
					})
				}).parse(caculation_inner)
			},
			_ => unreachable!(),
		}
	}

	fn handle_struct_assgin(&mut self, input: &Pair<Rule>, code: &String, start_line: usize) -> Result<(VarType, VarValue), Error> {
		debug!("defining struct object");
		let mut inner = input.clone().into_inner();
		let object_pair = inner.next().unwrap();
		let mut object_name = object_pair.as_str().to_string();
		if object_name == *"Self" {
			object_name = match &self.current_self_type {
				Some(inner) => inner.name(),
				None => return Err(Error::new_from_pair(ErrorType::UnexpectSelf, code, start_line, &object_pair)),
			}
		}
		let regstrated_objects = self.regstrated_objects.lock()?;
		let obj = if let Some(object) = regstrated_objects.get(&object_name) {
			if let Object::CustomObject(inside) = object {
				CustomObject {
					name: inside.name.clone(),
					fields: inside.fields.clone(),
				}
			}else {
				return Err(Error::new_from_pair(ErrorType::CantDefineRustObject(object_name), code, start_line, &object_pair));
			}
		}else {
			return Err(Error::new_from_pair(ErrorType::TypeNotFound(object_name), code, start_line, &object_pair));
		};
		drop(regstrated_objects);
		let mut fields = vec!();
		let mut elements = vec!();
		for field in inner {
			match field.as_rule() {
				Rule::VarName => {
					let field_name = field.as_str().to_string();
					if obj.fields.contains_key(&field_name) {
						fields.push(field_name);
					}else {
						return Err(Error::new_from_pair(ErrorType::SuperfluousField(object_name, field_name), code, start_line, &field));
					};
				},
				Rule::Element => {
					let var = self.handle_element(&field, code, start_line)?;
					if let Some(require_ty) = obj.fields.get(&fields[fields.len() - 1]) {
						if self.check_type_is_vaild(require_ty, &var.ty, &field, false)? {
							elements.push(Varible {
								name: fields[fields.len() - 1].clone(),
								ty: var.ty,
								value: var.value,
								nesting_times: 0,
							});
						}else {
							return Err(Error::new_from_pair(ErrorType::ExpectFound(require_ty.clone(), var.ty), code, start_line, &field));
						}
					}
				},
				_ => unreachable!(),
			}
		}
		let mut required_fields: HashSet<String> = obj.fields.keys().map(|inner| inner.to_string()).collect();
		for field in &fields {
			required_fields.remove(field);
		}
		if !required_fields.is_empty() {
			let mut missing_fields = String::new();
			for key in required_fields {
				missing_fields = format!("{missing_fields}{key}, ")
			}
			return Err(Error::new_from_pair(ErrorType::MissingField(object_name, missing_fields), code, start_line, input));
		}
		let mut field_map = HashMap::new();
		for (field, val) in fields.into_iter().zip(elements.into_iter()) {
			field_map.insert(field, val);
		}
		Ok((VarType::Object(object_name.clone()), VarValue::Object(ObjectValue::CustomObjectValue(CustomObjectValue {
			name: object_name,
			fields: field_map,
		}))))
	}

	fn handle_element_use(&mut self, input: &Pair<Rule>, code: &String, start_line: usize) -> Result<Condition, Error> {
		let mut input = input.clone().into_inner();
		let var_pair = input.next().unwrap();
		let var_name = var_pair.as_str().to_string();
		let used_varibles = self.used_varibles.lock()?;
		let var = if let Some(var) = used_varibles.get(&var_name) {
			var.clone()
		}else {
			return Err(Error::new_from_pair(ErrorType::VaribleNotFound(var_name), code, start_line, &var_pair));
		};
		drop(used_varibles);
		let mut used_varibles = HashMap::new();
		input.next();
		// println!("{:#?}", input);
		let mut function_pair = input.next().unwrap().into_inner();
		let function_name = function_pair.next().unwrap().as_str().to_string();
		let mut code_run = format!("{}::{}({}, ", var.ty.name(), function_name, var_name);
		used_varibles.insert(var_name.clone(), var);
		for (id, caculate) in function_pair.enumerate() {
			let calc = self.handle_element(&caculate, code, start_line)?;
			code_run = format!("{code_run}temp{id}, ");
			used_varibles.insert(format!("temp{id}"), calc);
		}
		code_run = format!("{code_run});");
		let mut sub_vm = self.clone();
		sub_vm.current_function_name = Some("complier function".to_string());
		sub_vm.used_varibles = Arc::new(Mutex::new(used_varibles));
		sub_vm.run_script(code_run)?;
		let used_varibles = sub_vm.used_varibles.lock()?;
		let var = used_varibles.get(&var_name).cloned().unwrap();
		drop(used_varibles);
		let mut used_varibles = self.used_varibles.lock()?;
		// println!("{:?}", var.value.partial_display());
		used_varibles.insert(var_name, var);
		Ok(Condition::None)
	}

	fn handle_visit_method_path(&mut self, input: &Pair<Rule>, code: &String, start_line: usize) -> Result<Condition, Error> {
		let mut inner = input.clone().into_inner();
		let type_pair = inner.next().unwrap();
		let ty = self.handle_type(&type_pair, code, start_line)?;
		let function_pair = inner.next().unwrap();
		self.handle_method(&function_pair, code, start_line, &ty)?;
		Ok(Condition::None)
	}

	fn check_type_is_vaild(&self, l: &VarType, r: &VarType, pair: &Pair<Rule>, allow_right_none: bool) -> Result<bool, Error> {
		debug!("checking assign type left: `{}`, right: `{}`", l.name(), r.name());
		let r = if r.is_array() {
			&VarType::ArrayAny
		}else {
			r
		};
		if let VarType::Result(l1, l2) = l {
			if let VarType::Result(r1, r2) = r {
				return Ok(self.check_type_is_vaild(l1, r1, pair, allow_right_none)? || self.check_type_is_vaild(l2, r2, pair, allow_right_none)?);
			}
		}
		let l = if l.is_self() {
			if let Some(ty) = &self.current_self_type {
				&ty.clone()
			}else {
				return Err(Error::new_from_pair(ErrorType::UnexpectSelf, &pair.as_str().to_string(), pair.line_col().0, pair));
			}
		}else {
			l
		};
		if l == r {
			return Ok(true);
		}
		if l.is_none() {
			return Ok(true);
		}
		if r.is_none() && allow_right_none {
			return Ok(true);
		}
		if let (VarType::Array(l), VarType::Array(r)) = (l, r) {
			return self.check_type_is_vaild(l, r, pair, true);
		}
		if let (VarType::Result(l1, l2), VarType::Result(r1, r2)) = (l, r) {
			return Ok(self.check_type_is_vaild(l1, r1, pair, true)? && self.check_type_is_vaild(l2, r2, pair, true)?);
		}
		if let VarType::Dynamic(l_name) = l {
			if AUTO_INTERFACE.contains(&l_name.as_str()) {
				return Ok(true);
			}
			if self.regstrated_interfaces.lock()?.get(l_name).is_none() {
				return Err(Error::new_from_pair(ErrorType::InterfaceNotFound(l_name.clone()), &pair.as_str().to_string(), pair.line_col().0, pair));
			}
			let mut interfaces = HashSet::new();
			if let Some(object) = self.implied_interfaces.lock()?.get(r) {
				for name in object.1.keys() {
					interfaces.insert(name.clone());
				}
			}else {
				return Err(Error::new_from_pair(ErrorType::TypeNotFound(r.name()), &pair.as_str().to_string(), pair.line_col().0, pair));
			}
			let implied_interfaces = self.implied_interfaces.lock()?;
			while let Some(element) = interfaces.iter().next().cloned() {
				interfaces.remove(&element);
				let name = element;
				if name == *l_name {
					return Ok(true);
				}
				if let Some((_, interface_map)) = implied_interfaces.get(&VarType::Dynamic(name)) {
					for name in interface_map.keys() {
						interfaces.insert(name.to_string());
					}
				}
			}
			return Ok(false);
		}
		Ok(false)
	}

	fn caculate(&mut self, input: Caculation, code: &String, start_line: usize, pair: &Pair<Rule>) -> Result<Varible, Error> {
		let var = match input.tree {
			CaculationTree::TempValue(ty, value) => {
				Varible {
					name: String::new(),
					ty,
					value,
					nesting_times: self.nesting_times,
				}
			},
			CaculationTree::Value(var_name) => {
				let var_name = var_name.trim().to_string();
				let mut used_varibles = self.used_varibles.lock()?;
				let the_value = used_varibles.get(&var_name);
				let cloned = if let Some(var) = the_value {
					var.clone()
				}else {
					return Err(Error::new_from_pair(ErrorType::VaribleNotFound(var_name), code, start_line, pair));
				};
				used_varibles.insert(var_name.clone(), cloned.clone());
				Varible {
					name: var_name.clone(),
					ty: cloned.ty,
					value: cloned.value,
					nesting_times: cloned.nesting_times,
				}
			},
			CaculationTree::MethodUse{ ty, total_code } => {
				let total_code = format!("{total_code};");
				self.current_function_name = Some("complier function".to_string());
				let parsed_data = NabloParser::parse(Rule::Statements, &total_code).unwrap().next().unwrap().into_inner().next().unwrap().into_inner().next().unwrap().into_inner().next().unwrap();
				let var = self.handle_method(&parsed_data, &parsed_data.as_str().to_string(), parsed_data.line_col().0, &ty)?;
				self.current_function_name = None;
				Varible {
					name: String::new(),
					ty: var.ty,
					value: var.value,
					nesting_times: self.nesting_times,
				}
			},
			CaculationTree::FunctionUse{ total_code } => {
				let total_code = format!("{total_code};");
				let parsed_data = NabloParser::parse(Rule::Statements, &total_code).unwrap().next().unwrap().into_inner().next().unwrap().into_inner().next().unwrap();
				self.current_function_name = Some("complier function".to_string());
				let var = self.handle_function(&parsed_data, &parsed_data.as_str().to_string(), parsed_data.line_col().0)?;
				self.current_function_name = None;
				Varible {
					name: String::new(),
					ty: var.ty,
					value: var.value,
					nesting_times: self.nesting_times,
				}
			},
			CaculationTree::Op{ lhs, op, rhs } => {
				let l = self.caculate(*lhs, code, start_line, pair)?;
				let r = self.caculate(*rhs, code, start_line, pair)?;
				let mut sub_vm = self.clone();
				let code = match op {
					Operation::Add => {
						if self.where_to_get_interface(&"Add".to_string(), &l.ty, pair)?.is_none() {
							return Err(Error::new_from_pair(ErrorType::InterfaceMissing(l.ty, "Add".to_string(), "cant add current values".to_string()), code, start_line, pair))
						}else {
							"out = l_temp.add(r_temp);"
						}
					},
					Operation::Minus => {
						if self.where_to_get_interface(&"Sub".to_string(), &l.ty, pair)?.is_none() {
							return Err(Error::new_from_pair(ErrorType::InterfaceMissing(l.ty, "Sub".to_string(), "cant sub current values".to_string()), code, start_line, pair))
						}else {
							"out = l_temp.sub(r_temp);"
						}
					},
					Operation::Cross => {
						if self.where_to_get_interface(&"Mul".to_string(), &l.ty, pair)?.is_none() {
							return Err(Error::new_from_pair(ErrorType::InterfaceMissing(l.ty, "Mul".to_string(), "cant multiply current values".to_string()), code, start_line, pair))
						}else {
							"out = l_temp.mul(r_temp);"
						}
					},
					Operation::Divid => {
						if self.where_to_get_interface(&"Div".to_string(), &l.ty, pair)?.is_none() {
							return Err(Error::new_from_pair(ErrorType::InterfaceMissing(l.ty, "Div".to_string(), "cant divid current values".to_string()), code, start_line, pair))
						}else {
							"out = l_temp.div(r_temp);"
						}
					},
					Operation::Power => {
						if self.where_to_get_interface(&"Pow".to_string(), &l.ty, pair)?.is_none() {
							return Err(Error::new_from_pair(ErrorType::InterfaceMissing(l.ty, "Pow".to_string(), "cant power current values".to_string()), code, start_line, pair))
						}else {
							"out = l_temp.pow(r_temp);"
						}
					},
					Operation::LargerOrEqualThan => {
						if self.where_to_get_interface(&"Cmp".to_string(), &l.ty, pair)?.is_none() {
							return Err(Error::new_from_pair(ErrorType::InterfaceMissing(l.ty, "Cmp".to_string(), "cant compare current values".to_string()), code, start_line, pair))
						}else {
							"out = l_temp.large_equal(r_temp);"
						}
					},
					Operation::LessOrEqualThan => {
						if self.where_to_get_interface(&"Cmp".to_string(), &l.ty, pair)?.is_none() {
							return Err(Error::new_from_pair(ErrorType::InterfaceMissing(l.ty, "Cmp".to_string(), "cant compare current values".to_string()), code, start_line, pair))
						}else {
							"out = l_temp.less_equal(r_temp);"
						}
					},
					Operation::LargeThan => {
						if self.where_to_get_interface(&"Cmp".to_string(), &l.ty, pair)?.is_none() {
							return Err(Error::new_from_pair(ErrorType::InterfaceMissing(l.ty, "Cmp".to_string(), "cant compare current values".to_string()), code, start_line, pair))
						}else {
							"out = l_temp.large(r_temp);"
						}
					},
					Operation::LessThan => {
						if self.where_to_get_interface(&"Cmp".to_string(), &l.ty, pair)?.is_none() {
							return Err(Error::new_from_pair(ErrorType::InterfaceMissing(l.ty, "Cmp".to_string(), "cant compare current values".to_string()), code, start_line, pair))
						}else {
							"out = l_temp.less(r_temp);"
						}
					},
					Operation::Unequal => {
						if self.where_to_get_interface(&"Eq".to_string(), &l.ty, pair)?.is_none() {
							return Err(Error::new_from_pair(ErrorType::InterfaceMissing(l.ty, "Eq".to_string(), "cant compare current values".to_string()), code, start_line, pair))
						}else {
							"out = l_temp.unequal(r_temp);"
						}
					},
					Operation::Same => {
						if self.where_to_get_interface(&"Eq".to_string(), &l.ty, pair)?.is_none() {
							return Err(Error::new_from_pair(ErrorType::InterfaceMissing(l.ty, "Eq".to_string(), "cant compare current values".to_string()), code, start_line, pair))
						}else {
							"out = l_temp.equal(r_temp);"
						}
					},
					Operation::And => {
						if self.where_to_get_interface(&"BitAnd".to_string(), &l.ty, pair)?.is_none() {
							return Err(Error::new_from_pair(ErrorType::InterfaceMissing(l.ty, "BitAnd".to_string(), "cant apply and on current values".to_string()), code, start_line, pair))
						}else {
							"out = l_temp.and(r_temp);"
						}
					},
					Operation::Or => {
						if self.where_to_get_interface(&"BitOr".to_string(), &l.ty, pair)?.is_none() {
							return Err(Error::new_from_pair(ErrorType::InterfaceMissing(l.ty, "BitOr".to_string(), "cant apply or on current values".to_string()), code, start_line, pair))
						}else {
							"out = l_temp.or(r_temp);"
						}
					},
				}.to_string();
				let used_varibles = HashMap::from([("l_temp".to_string(), l), ("r_temp".to_string(), r)]);
				sub_vm.used_varibles = Arc::new(Mutex::new(used_varibles));
				sub_vm.run_script(code)?;
				let used_varibles = sub_vm.used_varibles.lock()?;
				let out = used_varibles.get("out").cloned().unwrap();
				Varible {
					name: String::new(),
					..out
				}
			},
			CaculationTree::PreOp{is_neg, rhs} => {
				let r = self.caculate(*rhs, code, start_line, pair)?;
				let mut sub_vm = self.clone();
				let code = if is_neg {
					if self.where_to_get_interface(&"Neg".to_string(), &r.ty, pair)?.is_none() {
						return Err(Error::new_from_pair(ErrorType::InterfaceMissing(r.ty, "Neg".to_string(), "cant apply neg on current values".to_string()), code, start_line, pair))
					}else {
						"out = r_temp.neg();"
					}
				}else if self.where_to_get_interface(&"Not".to_string(), &r.ty, pair)?.is_none() {
					return Err(Error::new_from_pair(ErrorType::InterfaceMissing(r.ty, "Not".to_string(), "cant apply not on current values".to_string()), code, start_line, pair))
				}else {
					"out = r_temp.not();"
				};
				let used_varibles = HashMap::from([("r_temp".to_string(), r)]);
				sub_vm.used_varibles = Arc::new(Mutex::new(used_varibles));
				sub_vm.current_function_name = Some("complier function".to_string());
				sub_vm.run_script(code)?;
				let used_varibles = sub_vm.used_varibles.lock()?;
				let out = used_varibles.get("out").cloned().unwrap();
				Varible {
					name: String::new(),
					..out
				}
			},
		};
		let var = if let Some(inner) = input.get_value {
			let caculate = self.caculate(*inner, code, start_line, pair)?;
			let input_ty = if let VarType::Array(_) = var.ty {
				VarType::ArrayAny
			}else {
				var.ty.clone()
			};
			let code = if self.where_to_get_interface(&"Index".to_string(), &input_ty, pair)?.is_none() {
				return Err(Error::new_from_pair(ErrorType::InterfaceMissing(var.ty, "Index".to_string(), "cant apply index to current value".to_string()), code, start_line, pair))
			}else {
				"out = temp.index(ind);"
			};
			let used_varibles = HashMap::from([("temp".to_string(), var), ("ind".to_string(), caculate)]);
			let mut sub_vm = self.clone();
			sub_vm.used_varibles = Arc::new(Mutex::new(used_varibles));
			sub_vm.current_function_name = Some("complier function".to_string());
			sub_vm.run_script(code)?;
			let used_varibles = sub_vm.used_varibles.lock()?;
			let out = used_varibles.get("out").cloned().unwrap();
			Varible {
				name: String::new(),
				..out
			}
		}else {
			var
		};

		let var = if let Some(field_name) = input.visit_field {
			match var.value {
				VarValue::Object(ObjectValue::CustomObjectValue(inner)) => {
					if let Some(field) = inner.fields.get(&field_name) {
						field.clone()
					}else {
						return Err(Error::new_from_pair(ErrorType::FieldNotFound(var.ty, field_name), code, start_line, pair))
					}
				},
				VarValue::Object(ObjectValue::RustObjectValue(_)) => {
					return Err(Error::new_from_pair(ErrorType::CantVisitRustObject(var.ty.name()), code, start_line, pair))
				},
				_ => return Err(Error::new_from_pair(ErrorType::FieldNotFound(var.ty, field_name), code, start_line, pair))
			}
		}else {
			var
		};

		let var = if let Some((function_name, caculations)) = input.visit_function {
			let mut used_varibles = HashMap::new();
			// println!("{:?}", var.ty.name());
			let mut code_run = format!("{}::{function_name}(into, ", var.ty.name());
			for (id, _) in caculations.iter().enumerate() { 
				code_run = format!("{code_run}temp{id}, ")
			}
			code_run = format!("out = {code_run});");
			used_varibles.insert("into".to_string(), var);
			for (id, caculation) in caculations.into_iter().enumerate() {
				used_varibles.insert(format!("temp{}", id), self.caculate(caculation, code, start_line, pair)?);
			}
			let mut sub_vm = self.clone();
			sub_vm.used_varibles = Arc::new(Mutex::new(used_varibles));
			sub_vm.run_script(code_run)?;
			let used_varibles = sub_vm.used_varibles.lock()?;
			let out = used_varibles.get("out").cloned().unwrap();
			Varible {
				name: String::new(),
				..out
			}
		}else {
			var
		};
		Ok(var)
	}

	fn where_to_get_interface(&mut self, l_name: &String, ty: &VarType, pair: &Pair<Rule>) -> Result<Option<VarType>, Error> {
		if AUTO_INTERFACE.contains(&l_name.as_str()) {
			return Ok(Some(ty.clone()));
		}
		let ty = if ty.is_array() {
			&VarType::ArrayAny
		}else {
			ty
		};
		let ty = if ty.is_self() {
			if let Some(ty) = &self.current_self_type {
				&ty.clone()
			}else {
				return Err(Error::new_from_pair(ErrorType::UnexpectSelf, &pair.as_str().to_string(), pair.line_col().0, pair));
			}
		}else {
			ty
		};
		if self.regstrated_interfaces.lock()?.get(l_name).is_none() {
			return Ok(None);
		}
		let mut interfaces = HashSet::new();
		if let Some(object) = self.implied_interfaces.lock()?.get(ty) {
			for name in object.1.keys() {
				interfaces.insert((ty.clone(), name.clone()));
			}
		}else {
			return Err(Error::new_from_pair(ErrorType::TypeNotFound(ty.name()), &pair.as_str().to_string(), pair.line_col().0, pair));
		}
		let implied_interfaces = self.implied_interfaces.lock()?;
		while let Some(element) = interfaces.iter().next().cloned() {
			interfaces.remove(&element);
			let (inner_ty, name) = element;
			if name == *l_name {
				return Ok(Some(inner_ty));
			}
			if let Some((_, interface_map)) = implied_interfaces.get(&VarType::Dynamic(name)) {
				for name in interface_map.keys() {
					interfaces.insert((VarType::Dynamic(name.clone()), name.to_string()));
				}
			}
		}
		Ok(None)
	}

	fn field_start(&mut self) {
		debug!("starting a new field");
		self.nesting_times += 1;
	}

	fn field_end(&mut self) -> Result<(), Error> {
		debug!("ending a new field");
		let mut used_varibles = self.used_varibles.lock()?;
		used_varibles.retain(|_, var| {
			// println!("{}: {}, current: {}, {}", var.name, var.nesting_times, self.nesting_times, var.nesting_times <= self.nesting_times);
			var.nesting_times <= self.nesting_times
		});
		let mut regstrated_functions = self.regstrated_functions.lock()?;
		regstrated_functions.retain(|_, func| {
			func.nesting_times <= self.nesting_times
		});
		self.nesting_times -= 1;
		Ok(())
	}

	fn new_var(&mut self, var_name: String, ty: VarType, value: VarValue) -> Result<(), Error> {
		debug!("adding new varible `{var_name}`");
		self.used_varibles.lock()?.insert(var_name.clone(), Varible {
			name: var_name,
			ty,
			value,
			nesting_times: self.nesting_times
		});
		Ok(())
	}
}