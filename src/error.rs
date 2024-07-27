/*! Saves errors may meet while running script */

use crate::var::VarType;
use crate::Rule;
use std::sync::MutexGuard;
use std::sync::PoisonError;
use pest::error::ErrorVariant;
use pest::iterators::Pair;
use colored::*;
use std::fmt::Formatter;
use std::fmt::Display;

/// The error user can generate.
pub struct CustomError {
	/// describe what error it is
	pub describe: String,
	/// describe how to fix the problem
	pub suggestion: String,
}

impl<T, U> From<(T, U)> for CustomError 
where
	T: Into<String>,
	U: Into<String>
{
	fn from(input: (T, U)) -> Self {
		Self {
			describe: input.0.into(),
			suggestion: input.1.into(),
		}
	}
}

#[derive(Debug)]
/// Possible error types while running
pub enum ErrorType {
	GrammerError(Box<pest::error::Error<Rule>>),
	FunctionNotFound(String),
	FunctionExist(String),
	MethodExist(String),
	MacroNotFound(String),
	TypeNotFound(String),
	ExpectFound(VarType, VarType),
	InterfaceMissing(VarType, String, String),
	InterfaceNotFound(String),
	// AutoUnsupport(String),
	NonAsciiInput,
	NoInput,
	NotResult(VarType),
	UnexpectContinue,
	UnexpectBreak,
	UnexpectReturn,
	UnexpectSelf,
	// MissingBreak,
	VaribleAmountUnmatch(usize, usize),
	RuntimeMutexPoisoned,
	// FunctionMissingReturn(String),
	// MacroMissingReturn(String),
	VarExist(String),
	FieldExist(String),
	ObjectExist(String),
	SuperfluousFunction(String, String),
	CantImplyNone,
	MissingFunction(String, String),
	InterfaceExist(String),
	CantDefineRustObject(String),
	MissingField(String, String),
	FieldNotFound(VarType, String),
	SuperfluousField(String, String),
	VaribleNotFound(String),
	// CantCaculateRustyObject(String),
	MethodNotFound(String),
	CantVisitRustObject(String),
	UnsupportImply,
	Custom(String, String),
}

impl ErrorType {
	fn display_forward(&self, function: &Option<String>) -> String {
		let err = format!("{}: ", "error".red());
		let info = match self {
			Self::GrammerError(e) => {
				match &e.variant {
					ErrorVariant::ParsingError { positives, negatives } => {
						let mut find = String::new();
						if !positives.is_empty() {
							find = "expect: ".to_string();
						}
						for inner in positives {
							find = format!("{find}{:?},", inner)
						}
						if !negatives.is_empty() {
							find = "find: ".to_string();
						}
						for inner in negatives {
							find = format!("{find}{:?},", inner)
						}
						find
					},
					ErrorVariant::CustomError { message } => {
						message.to_string()
					},
				}
			},
			Self::FunctionNotFound(name) => format!("function `{name}` not found"),
			Self::MacroNotFound(name) => format!("macro `{name}` not found"),
			Self::ExpectFound(ty1, ty2) => format!("mismatched value type expect: `{}`, find: `{}`", ty1.name(), ty2.name()),
			Self::InterfaceMissing(ty, name, info) => format!("type `{}` missing required interface: `{}`, {info}", ty.name(), name),
			Self::NonAsciiInput => "found non-ascii code".to_string(),
			Self::NoInput => "nothing inputed".to_string(),
			Self::InterfaceNotFound(inner) => format!("interface `{inner}` not found"),
			// Self::AutoUnsupport(inner) => format!("interface `{inner}` not find"),
			Self::TypeNotFound(inner) => format!("type `{inner}` not found"),
			Self::NotResult(ty) => format!("asked non-result and non-option value, find: `{}`", ty.name()),
			Self::UnexpectContinue => "not in loop, but meet continue".to_string(),
			Self::UnexpectBreak => "not in loop, but meet break".to_string(),
			Self::UnexpectReturn => "not in function or macro, but meet return".to_string(),
			Self::UnexpectSelf => "not in impliment or interface state, but meet self".to_string(),
			// Self::MissingBreak => "exist infinite loop".to_string(),
			Self::VaribleAmountUnmatch(expect, find) => format!("element amount unmatch expect: `{expect}`, find: `{find}`"),
			Self::RuntimeMutexPoisoned => "runtime mutex poisoned".to_string(),
			// Self::FunctionMissingReturn(name) => format!("function `{}` missing return value", name),
			// Self::MacroMissingReturn(name) => format!("macro `{}!` missing return value", name),
			Self::VarExist(name) => format!("varible `{name}` already exists"),
			Self::FunctionExist(name) => format!("function `{name}` already exists"),
			Self::ObjectExist(name) => format!("object `{name}` already exists"),
			Self::FieldExist(name) => format!("field `{name}` already exists"),
			Self::SuperfluousFunction(interface, function) => format!("function `{function}` doesnt exist in interface `{interface}`"),
			Self::CantImplyNone => "cant imply `none` any method or trait".to_string(),
			Self::MissingFunction(interface, function) => format!("interface `{interface}` missing function `{function}`"),
			Self::MethodExist(name) => format!("method `{name}` already exists"),
			Self::InterfaceExist(name) => format!("interface `{name}` already exists"),
			Self::CantDefineRustObject(name) => format!("`{name}` is a rust object, rust object can only be defined by functions came from rust"),
			Self::MissingField(obj, name) => format!("object `{obj}` missing field `{name}`"),
			Self::SuperfluousField(obj, name) => format!("field `{name}` doesnt exist in object `{obj}`"),
			Self::VaribleNotFound(name) => format!("varible `{name}` not found"),
			// Self::CantCaculateRustyObject(name) => format!("`{name}` is a rust object or contains rust function, which cant engage caculation"),
			Self::MethodNotFound(name) => format!("method `{name}` not found"),
			Self::FieldNotFound(ty, name) => format!("could not found field `{name}` in {}", ty.name()),
			Self::CantVisitRustObject(name) => format!("`{name}` is a rust object, cant visit rust object's field"),
			Self::UnsupportImply => "imply a dynamic value, array value or result value is not supported for now".to_string(),
			Self::Custom(describe, _) => describe.to_string(),
		};
		if let Some(text) = function {
			format!("{err}{info}\nnote: using relative line while parsing function or macro `{text}`")
		}else {
			format!("{err}{info}")
		}
	}

	fn display(&self) -> String {
		let info = match self {
			Self::GrammerError(_) => return "help: TODO".to_string().yellow().to_string(),
			Self::FunctionNotFound(_) => "declare the function, change the pointed function or check your spell".to_string(),
			Self::MacroNotFound(_) => "declare the macro, change the pointed macro or check your spell".to_string(),
			Self::ExpectFound(_, _) => "change the pointed value or check your spell".to_string(),
			Self::InterfaceMissing(ty, name, _) => format!("imply interface {} for {}, replace current type or check your spell", name, ty.name()),
			Self::NonAsciiInput => "remove pointed character".to_string(),
			Self::NoInput => "add some codes".to_string(),
			Self::InterfaceNotFound(_) => "declare the interface, change the pointed interface or check your spell".to_string(),
			// Self::AutoUnsupport(_) => "interface can be auto implied for an object currently cant be modified, neigher do one who want inply it".to_string(),
			Self::TypeNotFound(_) => "declare the object, change the pointed type or check your spell".to_string(),
			Self::NotResult(_) => "change the pointed element, reconstruct your logic or check your spell".to_string(),
			Self::UnexpectContinue | Self::UnexpectBreak | Self::UnexpectReturn | Self::UnexpectSelf => "remove pointed statement".to_string(),
			// Self::MissingBreak => "add a break statement, loop statement need at least one break or there will be infinite loop".to_string(),
			Self::VaribleAmountUnmatch(expect, find) => {
				if expect > find {
					"add more input element or check your spell".to_string()
				}else {
					"remove superfluous input element or check your spell".to_string()
				}
			},
			Self::RuntimeMutexPoisoned => "this is not your fault".to_string(),
			// Self::FunctionMissingReturn(_) | Self::MacroMissingReturn(_) => format!("add return statement, assign value to varible `return` or check your logic"),
			Self::VarExist(_) => "remove superfluous varible, rename it or check your spell".to_string(),
			Self::FunctionExist(_) => "remove superfluous function, rename it or check your spell".to_string(),
			Self::ObjectExist(_) => "remove superfluous object, rename it or check your spell".to_string(),
			Self::FieldExist(_) => "remove superfluous field, rename it or check your spell".to_string(),
			Self::SuperfluousFunction(_, _) => "remove pointed function".to_string(),
			Self::CantImplyNone => "remove the imply statement".to_string(),
			Self::MissingFunction(_, _) => "add missing function".to_string(),
			Self::MethodExist(_) => "remove the method or check if other interface have used that function name".to_string(),
			Self::InterfaceExist(_) => "remove superfluous interface, rename it or check your spell".to_string(),
			Self::CantDefineRustObject(_) => "use object construct function instead".to_string(),
			Self::MissingField(_, _) => "add missing field".to_string(),
			Self::SuperfluousField(_, _) => "remove pointed field".to_string(),
			Self::VaribleNotFound(_) => "declare the varible, change the pointed varible or check your spell".to_string(),
			// Self::CantCaculateRustyObject(_) => "if necessary, use interface method instead or just remove it".to_string(),
			Self::MethodNotFound(_) => "declare the method, change the pointed method or check your spell".to_string(),
			Self::FieldNotFound(_, _) => "check your spell".to_string(),
			Self::CantVisitRustObject(_) => "remove it or check your spell".to_string(),
			Self::UnsupportImply => "remove it".to_string(),
			Self::Custom(_, suggestion) => return format!("help: {}", suggestion).yellow().to_string(),
		};

		format!("{}{}{}","help: try to ".yellow(), info.yellow(), " to aviod current error".yellow())
	}
}

impl From<CustomError> for ErrorType {
	fn from(input: CustomError) -> Self {
		ErrorType::Custom(input.describe, input.suggestion)
	}
}

#[derive(Debug)]
/// The error which `nablo_laguage` deliver, with a highly readable error info.
///
/// Note: currently [`Error`] cant display 100% satisfying, since there will be some spcial code generate by [`crate::VisualMachine`].
pub struct Error {
	pub(crate) error_type: ErrorType,
	pub(crate) code: String,
	pub(crate) start_line: usize,
	pub(crate) error_line: usize,
	pub(crate) error_col: usize,
	pub(crate) error_length: usize,
	pub(crate) function: Option<String>,
}

impl Error {
	pub(crate) fn new_from_pair(error_type: ErrorType, code: &String, start_line: usize, pair: &Pair<'_, Rule>) -> Self {
		let line_col = pair.line_col();
		let span = pair.as_span();
		let length = span.end() - span.start();
		Self {
			error_type,
			code: code.to_string(),
			start_line,
			error_line: line_col.0,
			error_col: line_col.1,
			error_length: length,
			function: None,
		}
	}

	pub(crate) fn new_empty(error_type: ErrorType) -> Self {
		Self {
			error_type,
			code: String::new(),
			start_line: 0,
			error_line: 0,
			error_col: 0,
			error_length: 1,
			function: None,
		}
	}

	pub(crate) fn new_from_pest(error: pest::error::Error<Rule>, total_code: &str) -> Self {
		let (line_col, mut length) = match error.line_col {
			pest::error::LineColLocation::Pos((line, col)) => {
				((line, col), error.line().len().checked_sub(col).unwrap_or(1))
			},
			pest::error::LineColLocation::Span((line_1, col_1), (line_2, col_2)) => {
				let line_1_len = line_1.to_string().len();
				let line_2_len = line_2.to_string().len();
				let space = col_2 - col_1;
				if line_1_len > line_2_len {
					((line_1, col_1), space)
				}else {
					((line_2, col_2), space)
				}
			}
		};
		let mut code = String::new();
		let mut code_to_get = vec!(line_col.0.saturating_sub(1), line_col.0, line_col.0 + 1);
		code_to_get.retain(|i| *i > 0);
		for (id, line) in total_code.lines().enumerate() {
			if code_to_get.contains(&(id + 1)) {
				code = format!("{code}{}\n", line)
			}
		}
		if length < 1 {
			length = 1;
		}
		Self {
			code,
			start_line: code_to_get[0],
			error_line: line_col.0,
			error_col: line_col.1,
			error_length: length,
			error_type: ErrorType::GrammerError(Box::new(error)),
			function: None,
		}
	}
}

impl From<ErrorType> for Error {
	fn from(input: ErrorType) -> Self {
		Self::new_empty(input)
	}
}

impl From<(pest::error::Error<Rule>, &String)> for Error {
	fn from(input: (pest::error::Error<Rule>, &String)) -> Self {
		Self::new_from_pest(input.0, input.1)
	}
}

impl<T: ?Sized> From<PoisonError<MutexGuard<'_, T>>> for Error {
	fn from(_: PoisonError<MutexGuard<'_, T>>) -> Self {
		ErrorType::RuntimeMutexPoisoned.into()
	}
}

impl Display for Error {
	fn fmt(&self, f: &mut Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
		let the_break = String::from("|").blue();
		let pointer = String::from("=>").blue();
		let split = self.code.lines();
		let space_len = (self.start_line + split.clone().count()).to_string().len();
		let space: String = (0..space_len).map(|_| " ").collect();
		let mut code_line = String::new();
		for (id, line) in split.enumerate() {
			let current_line = self.start_line + id;
			let post_space: String = (0..space_len - current_line.to_string().len()).map(|_| " ").collect();
			let current_line = current_line.to_string().blue();
			if line.trim().to_string().is_empty() {
				continue;
			}
			if self.start_line + id == self.error_line {
				let err_space: String = (0..self.error_col.saturating_sub(1)).map(|_| " ").collect();
				let length = if self.error_length > line.len() - self.error_col.saturating_sub(1) {
					line.len() - self.error_col.saturating_sub(1)
				}else {
					self.error_length
				};
				let length = if length < 1 {
					1
				}else {
					length
				};
				let err_pointer:String = (0..length).map(|_| "^").collect();
				code_line = format!("{code_line}{post_space}{0} {the_break} {1}\n", current_line, line);
				code_line = format!("{code_line}{space} {the_break} {err_space}{0}\n", err_pointer.red());
			}else {
				code_line = format!("{0}{post_space}{1} {the_break} {2}\n", code_line, current_line, line);
			}
		}
		code_line = code_line.trim_end().to_string();
		write!(f, "{0}\n{space} {the_break}\n{code_line}\n{space} {the_break}\n{space}{pointer} {1}", self.error_type.display_forward(&self.function) ,self.error_type.display())
	}
}

impl std::error::Error for Error {}