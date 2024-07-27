//! Here saves `basic_lib` and `standard_lib`'s functions

use std::collections::HashMap;
use crate::*;

/// the `type` function in interface `Any` 
pub fn any_type(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let input = input.get("self").unwrap();
	Ok(Varible {
		ty: VarType::String,
		value: VarValue::String(input.ty.name()),
		..Default::default()
	})
}

/// the `is_same_type` function in interface `Any`
pub fn any_is_same_type(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let inner = input.get("self").unwrap();
	let other = input.get("other").unwrap();
	Ok(Varible {
		ty: VarType::Bool,
		value: VarValue::Bool(inner.ty.name() == other.ty.name()),
		..Default::default()
	})
}

/// the `downcast_to_other` function in interface `Any`
pub fn any_downcast_to_other(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let inner = input.get("self").unwrap();
	let other = input.get("other").unwrap();
	let out = match &inner.ty {
		VarType::None => {
			match other.ty {
				VarType::None => Ok(Box::new(VarValue::None)),
				VarType::Number => Ok(Box::new(VarValue::Number(0.0))),
				VarType::Bool => Ok(Box::new(VarValue::Bool(false))),
				VarType::String => Ok(Box::new(VarValue::String("none".to_string()))),
				VarType::Result(_, _) => Ok(Box::new(VarValue::Result(Ok(Box::new(VarValue::None))))),
				VarType::Array(_) => Ok(Box::new(VarValue::Array(vec!(VarValue::None)))),
				_ => Err(Box::new(VarValue::String(format!("cant convert a `{}` value into `{}`", inner.ty.name(), other.ty.name())))),
			}
		},
		VarType::Number => {
			let inner_value = if let VarValue::Number(inside) = inner.value {
				inside
			}else {
				unreachable!()
			};
			match other.ty {
				VarType::None => Ok(Box::new(VarValue::None)),
				VarType::Number => Ok(Box::new(VarValue::Number(inner_value))),
				VarType::Bool => Ok(Box::new(VarValue::Bool(inner_value == 0.0))),
				VarType::String => Ok(Box::new(VarValue::String(inner_value.to_string()))),
				VarType::Result(_, _) => Ok(Box::new(VarValue::Result(Ok(Box::new(VarValue::Number(inner_value)))))),
				VarType::Array(_) => Ok(Box::new(VarValue::Array(vec!(VarValue::Number(inner_value))))),
				_ => Err(Box::new(VarValue::String(format!("cant convert a `{}` value into `{}`", inner.ty.name(), other.ty.name())))),
			}

		},
		VarType::Bool => {
			let inner_value = if let VarValue::Bool(inside) = inner.value {
				inside
			}else {
				unreachable!()
			};
			match other.ty {
				VarType::None => Ok(Box::new(VarValue::None)),
				VarType::Number => Ok(Box::new(VarValue::Number(if inner_value {
					1.0
				}else {
					0.0
				}))),
				VarType::Bool => Ok(Box::new(VarValue::Bool(inner_value))),
				VarType::String => Ok(Box::new(VarValue::String(inner_value.to_string()))),
				VarType::Result(_, _) => Ok(Box::new(VarValue::Result(Ok(Box::new(VarValue::Bool(inner_value)))))),
				VarType::Array(_) => Ok(Box::new(VarValue::Array(vec!(VarValue::Bool(inner_value))))),
				_ => Err(Box::new(VarValue::String(format!("cant convert a `{}` value into `{}`", inner.ty.name(), other.ty.name())))),
			}
		},
		VarType::String => {
			let inner_value = if let VarValue::String(inside) = &inner.value {
				inside.clone()
			}else {
				unreachable!()
			};
			match other.ty {
				VarType::None => Ok(Box::new(VarValue::None)),
				VarType::Number => Ok(Box::new(VarValue::Number(inner_value.parse().unwrap_or(f64::NAN)))),
				VarType::Bool => {
					if inner_value.trim() == "true" {
						Ok(Box::new(VarValue::Bool(true)))
					}else if inner_value.trim() == "false" {
						Ok(Box::new(VarValue::Bool(false)))
					}else {
						Err(Box::new(VarValue::String(format!("cant convert a `{}` value into `{}`", inner.ty.name(), other.ty.name()))))
					}
				},
				VarType::String => Ok(Box::new(VarValue::String(inner_value.to_string()))),
				VarType::Result(_, _) => Ok(Box::new(VarValue::Result(Ok(Box::new(VarValue::String(inner_value)))))),
				VarType::Array(_) => Ok(Box::new(VarValue::Array(vec!(VarValue::String(inner_value))))),
				_ => Err(Box::new(VarValue::String(format!("cant convert a `{}` value into `{}`", inner.ty.name(), other.ty.name())))),
			}
		},
		_ => {
			let inner_value = inner.value.clone(); 
			match other.ty {
				VarType::None => Ok(Box::new(VarValue::None)),
				VarType::Result(_, _) => {
					Ok(Box::new(VarValue::Result(Ok(Box::new(inner_value)))))
				},
				VarType::Array(_) => {
					Ok(Box::new(VarValue::Array(vec!(inner_value))))
				},
				_ => Err(Box::new(VarValue::String(format!("cant downcast a `{}` value into `{}`", inner.ty.name(), other.ty.name())))),
			}
		},
	};
	Ok(Varible {
		ty: VarType::Result(Box::new(other.ty.clone()), Box::new(VarType::String)),
		value: VarValue::Result(out),
		..Default::default()
	})
}

/// the `add` function in interface `Add`
pub fn add_add(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let lhs = input.get("self").unwrap();
	let rhs = input.get("rhs").unwrap();
	if let (VarValue::Number(lhs), VarValue::Number(rhs)) = (&lhs.value, &rhs.value) {
		Ok(Varible {
			ty: VarType::Number,
			value: VarValue::Number(lhs + rhs),
			..Default::default()
		})
	}else if let (VarValue::String(lhs), VarValue::String(rhs)) = (&lhs.value, &rhs.value)  {
		Ok(Varible {
			ty: VarType::String,
			value: VarValue::String(format!("{lhs}{rhs}")),
			..Default::default()
		})
	}else {
		Err((format!("cant add non-num values or non-string values, found lhs: {}, rhs: {}", lhs.ty, rhs.ty), "check your logic or spell").into())
	}
}

/// the `sub` function in interface `Sub`
pub fn sub_sub(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let lhs = input.get("self").unwrap();
	let rhs = input.get("rhs").unwrap();
	if let (VarValue::Number(lhs), VarValue::Number(rhs)) = (&lhs.value, &rhs.value) {
		Ok(Varible {
			ty: VarType::Number,
			value: VarValue::Number(lhs - rhs),
			..Default::default()
		})
	}else {
		Err((format!("cant minus non-num values, found lhs: {}, rhs: {}", lhs.ty, rhs.ty), "check your logic or spell").into())
	}
}

/// the `mul` function in interface `Mul`
pub fn mul_mul(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let lhs = input.get("self").unwrap();
	let rhs = input.get("rhs").unwrap();
	if let (VarValue::Number(lhs), VarValue::Number(rhs)) = (&lhs.value, &rhs.value) {
		Ok(Varible {
			ty: VarType::Number,
			value: VarValue::Number(lhs * rhs),
			..Default::default()
		})
	}else {
		Err((format!("cant multiply non-num values, found lhs: {}, rhs: {}", lhs.ty, rhs.ty), "check your logic or spell").into())
	}
}

/// the `div` function in interface `Div`
pub fn div_div(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let lhs = input.get("self").unwrap();
	let rhs = input.get("rhs").unwrap();
	if let (VarValue::Number(lhs), VarValue::Number(rhs)) = (&lhs.value, &rhs.value) {
		Ok(Varible {
			ty: VarType::Number,
			value: VarValue::Number(lhs / rhs),
			..Default::default()
		})
	}else {
		Err((format!("cant divide non-num values, found lhs: {}, rhs: {}", lhs.ty, rhs.ty), "check your logic or spell").into())
	}
}

/// the `pow` function in interface `Pow`
pub fn pow_pow(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let lhs = input.get("self").unwrap();
	let rhs = input.get("rhs").unwrap();
	if let (VarValue::Number(lhs), VarValue::Number(rhs)) = (&lhs.value, &rhs.value) {
		Ok(Varible {
			ty: VarType::Number,
			value: VarValue::Number(lhs.powf(*rhs)),
			..Default::default()
		})
	}else {
		Err((format!("cant apply power caculator on non-num values, found lhs: {}, rhs: {}", lhs.ty, rhs.ty), "check your logic or spell").into())
	}
}

/// the `large_equal` function in interface `Cmp`
pub fn cmp_large_equal(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let lhs = input.get("self").unwrap();
	let rhs = input.get("rhs").unwrap();
	if let (VarValue::Number(lhs), VarValue::Number(rhs)) = (&lhs.value, &rhs.value) {
		Ok(Varible {
			ty: VarType::Bool,
			value: VarValue::Bool(lhs >= rhs),
			..Default::default()
		})
	}else {
		Err((format!("cant compare two non-num values, found lhs: {}, rhs: {}", lhs.ty, rhs.ty), "check your logic or spell").into())
	}
}

/// the `less_equal` function in interface Cmp`
pub fn cmp_less_equal(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let lhs = input.get("self").unwrap();
	let rhs = input.get("rhs").unwrap();
	if let (VarValue::Number(lhs), VarValue::Number(rhs)) = (&lhs.value, &rhs.value) {
		Ok(Varible {
			ty: VarType::Bool,
			value: VarValue::Bool(lhs <= rhs),
			..Default::default()
		})
	}else {
		Err((format!("cant compare two non-num values, found lhs: {}, rhs: {}", lhs.ty, rhs.ty), "check your logic or spell").into())
	}
}

/// the `large` function in interface `Cmp`
pub fn cmp_large(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let lhs = input.get("self").unwrap();
	let rhs = input.get("rhs").unwrap();
	if let (VarValue::Number(lhs), VarValue::Number(rhs)) = (&lhs.value, &rhs.value) {
		Ok(Varible {
			ty: VarType::Bool,
			value: VarValue::Bool(lhs > rhs),
			..Default::default()
		})
	}else {
		Err((format!("cant compare two non-num values, found lhs: {}, rhs: {}", lhs.ty, rhs.ty), "check your logic or spell").into())
	}
}

/// the `large_equal` function in interface `Cmp`
pub fn cmp_less(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let lhs = input.get("self").unwrap();
	let rhs = input.get("rhs").unwrap();
	if let (VarValue::Number(lhs), VarValue::Number(rhs)) = (&lhs.value, &rhs.value) {
		Ok(Varible {
			ty: VarType::Bool,
			value: VarValue::Bool(lhs < rhs),
			..Default::default()
		})
	}else {
		Err((format!("cant compare two non-num values, found lhs: {}, rhs: {}", lhs.ty, rhs.ty), "check your logic or spell").into())
	}
}

/// the `euqal` function ininterface `Eq`
pub fn eq_equal(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let lhs = input.get("self").unwrap();
	let rhs = input.get("rhs").unwrap();
	fn cmp_all(lhs: &VarValue, rhs: &VarValue, lty: &VarType, rty: &VarType) -> Result<bool, CustomError> {
		match (lhs, rhs) {
			(VarValue::None, VarValue::None) => Ok(true),
			(VarValue::Number(lhs), VarValue::Number(rhs)) => Ok(lhs == rhs),
			(VarValue::Bool(lhs), VarValue::Bool(rhs)) => Ok(lhs == rhs),
			(VarValue::String(lhs), VarValue::String(rhs)) => Ok(lhs == rhs),
			(VarValue::Result(lhs), VarValue::Result(rhs)) => {
				let lhs = match lhs {
					Ok(lhs) => lhs.clone(),
					Err(lhs) => lhs.clone(),
				};
				let rhs = match rhs {
					Ok(rhs) => rhs.clone(),
					Err(rhs) => rhs.clone(),
				};
				cmp_all(&lhs, &rhs, &lhs.get_type(), &rhs.get_type())
			},
			(VarValue::Array(lhs), VarValue::Array(rhs)) => {
				let mut out = true;
				for (lhs, rhs) in lhs.iter().zip(rhs.iter()) {
					let lhs = lhs.clone();
					let rhs = rhs.clone();
					out = out && cmp_all(&lhs, &rhs, &lhs.get_type(), &rhs.get_type())?;
				}
				Ok(out)
			},
			_ => Err((format!("cant compare two current values, found lhs: {}, rhs: {}", lty, rty), "check your logic or spell").into()),
		}
	}
	let out = cmp_all(&lhs.value, &rhs.value, &lhs.ty, &rhs.ty)?;
	Ok(Varible {
		ty: VarType::Bool,
		value: VarValue::Bool(out),
		..Default::default()
	})
}

/// the `unequal` function ininterface `Eq`
pub fn eq_unequal(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let lhs = input.get("self").unwrap();
	let rhs = input.get("rhs").unwrap();
	fn cmp_all(lhs: &VarValue, rhs: &VarValue, lty: &VarType, rty: &VarType) -> Result<bool, CustomError> {
		match (lhs, rhs) {
			(VarValue::None, VarValue::None) => Ok(true),
			(VarValue::Number(lhs), VarValue::Number(rhs)) => Ok(lhs == rhs),
			(VarValue::Bool(lhs), VarValue::Bool(rhs)) => Ok(lhs == rhs),
			(VarValue::String(lhs), VarValue::String(rhs)) => Ok(lhs == rhs),
			(VarValue::Result(lhs), VarValue::Result(rhs)) => {
				let lhs = match lhs {
					Ok(lhs) => lhs.clone(),
					Err(lhs) => lhs.clone(),
				};
				let rhs = match rhs {
					Ok(rhs) => rhs.clone(),
					Err(rhs) => rhs.clone(),
				};
				cmp_all(&lhs, &rhs, &lhs.get_type(), &rhs.get_type())
			},
			(VarValue::Array(lhs), VarValue::Array(rhs)) => {
				let mut out = true;
				for (lhs, rhs) in lhs.iter().zip(rhs.iter()) {
					let lhs = lhs.clone();
					let rhs = rhs.clone();
					out = out && cmp_all(&lhs, &rhs, &lhs.get_type(), &rhs.get_type())?;
				}
				Ok(out)
			},
			_ => Err((format!("cant compare two current values, found lhs: {}, rhs: {}", lty, rty), "check your logic or spell").into()),
		}
	}
	let out = cmp_all(&lhs.value, &rhs.value, &lhs.ty, &rhs.ty)?;
	Ok(Varible {
		ty: VarType::Bool,
		value: VarValue::Bool(!out),
		..Default::default()
	})
}

/// the `and` function in interface `BitAnd`
pub fn bit_and_and(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let lhs = input.get("self").unwrap();
	let rhs = input.get("rhs").unwrap();
	if let (VarValue::Bool(lhs), VarValue::Bool(rhs)) = (&lhs.value, &rhs.value) {
		Ok(Varible {
			ty: VarType::Bool,
			value: VarValue::Bool(*lhs && *rhs),
			..Default::default()
		})
	}else {
		Err((format!("cant apply power caculator on non-boolean values, found lhs: {}, rhs: {}", lhs.ty, rhs.ty), "check your logic or spell").into())
	}
}

/// the `or` function in interface `BitOr`
pub fn bit_or_or(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let lhs = input.get("self").unwrap();
	let rhs = input.get("rhs").unwrap();
	if let (VarValue::Bool(lhs), VarValue::Bool(rhs)) = (&lhs.value, &rhs.value) {
		Ok(Varible {
			ty: VarType::Bool,
			value: VarValue::Bool(*lhs || *rhs),
			..Default::default()
		})
	}else {
		Err((format!("cant apply power caculator on non-boolean values, found lhs: {}, rhs: {}", lhs.ty, rhs.ty), "check your logic or spell").into())
	}
}

/// the `neg` function in interface `Neg`
pub fn neg_neg(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let lhs = input.get("self").unwrap();
	if let VarValue::Number(lhs) = &lhs.value {
		Ok(Varible {
			ty: VarType::Number,
			value: VarValue::Number(-lhs),
			..Default::default()
		})
	}else {
		Err((format!("cant apply power caculator on non-boolean values, found rhs: {}", lhs.ty), "check your logic or spell").into())
	}
}

/// the `not` function in interface `Not`
pub fn not_not(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let lhs = input.get("self").unwrap();
	if let VarValue::Bool(lhs) = &lhs.value {
		Ok(Varible {
			ty: VarType::Bool,
			value: VarValue::Bool(!lhs),
			..Default::default()
		})
	}else {
		Err((format!("cant apply power caculator on non-boolean values, found rhs: {}", lhs.ty), "check your logic or spell").into())
	}
}

/// the `index` function in interface `Index`
pub fn index_index(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let id = {
		let index = input.get_mut("index").unwrap();
		if let VarValue::Number(id) = index.value {
			if id < 0.0 {
				0
			}else {
				id.round() as usize
			}
		}else {
			return Err((format!("cant use non-number value as index, found: {}", index.ty), "check your logic or spell").into());
		}
	};
	let input = input.get_mut("self").unwrap();
	if let VarValue::Array(inner) = &mut input.value {
		if id + 1 > inner.len() {
			Err((format!("the index value is larger than current array's len, len: {}, find: {}", inner.len(), id), "check your logic or spell").into())
		}else {
			let vec_value = inner.remove(id);
			inner.insert(id, vec_value.clone());
			let val = vec_value;
			Ok(Varible {
				ty: val.get_type(),
				value: val,
				..Default::default()
			})
		}
	}else {
		Err((format!("cant apply index on non-array values, found: {}", input.ty), "check your logic or spell").into())
	}
}

/// the `println` function
pub fn println(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let input = input.get("input").unwrap();
	if let Some(input) = input.value.partial_display() {
		println!("{}", input);
		Ok(Default::default())
	}else {
		Err((format!("current element's type is `{}`, which cant be printed", input.ty), "change the varible or check your spell").into())
	}
}

/// the `print` function
pub fn print(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let input = input.get("input").unwrap();
	if let Some(input) = input.value.partial_display() {
		print!("{}", input);
		Ok(Default::default())
	}else {
		Err((format!("current element's type is `{}`, which cant be printed", input.ty), "change the varible or check your spell").into())
	}
}

/// the `format` function
pub fn format(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let input = input.get("input").unwrap();
	if let Some(input) = input.value.partial_display() {
		Ok(Varible {
			ty: VarType::String,
			value: VarValue::String(input),
			..Default::default()
		})
	}else {
		Err((format!("current element's type is `{}`, which cant be formatted", input.ty), "change the varible or check your spell").into())
	}
}

/// the `compile_error` function
pub fn compile_error(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let description = input.get("description").unwrap();
	let suggestion = input.get("suggestion").unwrap();
	let description = if let Some(input) = description.value.partial_display() {
		input
	}else {
		return Err((format!("current element's type is `{}`, which cant be printed", description.ty), "change the varible or check your spell").into())
	};
	let suggestion = if let Some(input) = suggestion.value.partial_display() {
		input
	}else {
		return Err((format!("current element's type is `{}`, which cant be printed", suggestion.ty), "change the varible or check your spell").into())
	};
	Err((description, suggestion).into())
}

/// the `println` macro
pub fn macro_println(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let input = input.get("input").unwrap();
	if let VarValue::Array(inner) = &input.value {
		let mut out = String::new();
		let len = inner.len();
		for (id, caculation) in inner.iter().enumerate() {
			if let VarValue::String(caculation) = caculation {
				if len == id + 1 {
					out = format!("{out}println(format({}));\n", caculation);
				}else{
					out = format!("{out}print(format({}) + \", \");\n", caculation);
				}
			}else {
				unreachable!()
			}
		}
		Ok(Varible {
			ty: VarType::String,
			value: VarValue::String(out),
			..Default::default()
		})
	}else {
		unreachable!()
	}
}

/// the `print` macro
pub fn macro_print(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let input = input.get("input").unwrap();
	if let VarValue::Array(inner) = &input.value {
		let mut out = String::new();
		let len = inner.len();
		for (id, caculation) in inner.iter().enumerate() {
			if let VarValue::String(caculation) = caculation {
				if len == id + 1 {
					out = format!("{out}print(format({}));\n", caculation);
				}else{
					out = format!("{out}print(format({}) + \", \");\n", caculation);
				}
			}else {
				unreachable!()
			}
		}
		Ok(Varible {
			ty: VarType::String,
			value: VarValue::String(out),
			..Default::default()
		})
	}else {
		unreachable!()
	}
}

/// the `get_slice` method for `str`
pub fn method_string_get_slice(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let inner = input.get("self").unwrap();
	let l = input.get("l").unwrap();
	let r = input.get("r").unwrap();
	if let VarValue::String(inner) = &inner.value {
		if let (VarValue::Number(l), VarValue::Number(r)) = (&l.value, &r.value) {
			let l = if *l < 0.0 {
				0
			}else {
				l.round() as usize
			};
			let r = if *r < 0.0 {
				0
			}else {
				r.round() as usize
			};
			if (l + 1 > inner.len()) || (r + 1 > inner.len()) {
				Err((format!("the current range is not a sub set of string, found l: {}, r: {}, while the len of current string is {}", l, r, inner.len()), "check your logic, change the varible or check your spell").into())
			}else {
				Ok(Varible {
					ty: VarType::String,
					value: VarValue::String(inner[l..=r].to_string()), 
					..Default::default()
				})
			}
		}else {
			Err((format!("the `l` and `r` must be number, found l: {}, r: {}", l.ty, r.ty), "check your logic, change the varible or check your spell").into())
		}
	}else {
		Err(("cant use `get_slice` method on non-string value", "check your logic, change the varible or check your spell").into())
	}
}

/// the `len` method for `str` and `array`
pub fn method_len(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let inner = input.get("self").unwrap();
	if let VarValue::String(inner) = &inner.value {
		Ok(Varible {
			ty: VarType::Number,
			value: VarValue::Number(inner.len() as f64),
			..Default::default()
		})
	}else if let VarValue::Array(inner) = &inner.value {
		Ok(Varible {
			ty: VarType::Number,
			value: VarValue::Number(inner.len() as f64),
			..Default::default()
		})
	}else {
		Err(("cant use `len` method on non-string and non-array value", "check your logic, change the varible or check your spell").into())
	}
}

/// the `floor` method for `number`
pub fn method_number_floor(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let inner = input.get("self").unwrap();
	if let VarValue::Number(inner) = &inner.value {
		Ok(Varible {
			ty: VarType::Number,
			value: VarValue::Number(inner.floor()),
			..Default::default()
		})
	}else {
		Err(("cant use `floor` method on non-number value", "check your logic, change the varible or check your spell").into())
	}
}

/// the `ceil` method for `number`
pub fn method_number_ceil(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let inner = input.get("self").unwrap();
	if let VarValue::Number(inner) = &inner.value {
		Ok(Varible {
			ty: VarType::Number,
			value: VarValue::Number(inner.ceil()),
			..Default::default()
		})
	}else {
		Err(("cant use `ceil` method on non-number value", "check your logic, change the varible or check your spell").into())
	}
}

/// the `round` method for `number`
pub fn method_number_round(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let inner = input.get("self").unwrap();
	if let VarValue::Number(inner) = &inner.value {
		Ok(Varible {
			ty: VarType::Number,
			value: VarValue::Number(inner.round()),
			..Default::default()
		})
	}else {
		Err(("cant use `round` method on non-number value", "check your logic, change the varible or check your spell").into())
	}
}

/// the `round_ties_even` method for `number`
pub fn method_number_round_ties_even(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let inner = input.get("self").unwrap();
	if let VarValue::Number(inner) = &inner.value {
		Ok(Varible {
			ty: VarType::Number,
			value: VarValue::Number(inner.round_ties_even()),
			..Default::default()
		})
	}else {
		Err(("cant use `round_ties_even` method on non-number value", "check your logic, change the varible or check your spell").into())
	}
}

/// the `int_part` method for `number`
pub fn method_number_int_part(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let inner = input.get("self").unwrap();
	if let VarValue::Number(inner) = &inner.value {
		Ok(Varible {
			ty: VarType::Number,
			value: VarValue::Number(inner.trunc()),
			..Default::default()
		})
	}else {
		Err(("cant use `int_part` method on non-number value", "check your logic, change the varible or check your spell").into())
	}
}

/// the `fract_part` method for `number`
pub fn method_number_fract_part(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let inner = input.get("self").unwrap();
	if let VarValue::Number(inner) = &inner.value {
		Ok(Varible {
			ty: VarType::Number,
			value: VarValue::Number(inner.fract()),
			..Default::default()
		})
	}else {
		Err(("cant use `fract_part` method on non-number value", "check your logic, change the varible or check your spell").into())
	}
}

/// the `abs` method for `number`
pub fn method_number_abs(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let inner = input.get("self").unwrap();
	if let VarValue::Number(inner) = &inner.value {
		Ok(Varible {
			ty: VarType::Number,
			value: VarValue::Number(inner.abs()),
			..Default::default()
		})
	}else {
		Err(("cant use `abs` method on non-number value", "check your logic, change the varible or check your spell").into())
	}
}

/// the `sign` method for `number`
pub fn method_number_sign(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let inner = input.get("self").unwrap();
	if let VarValue::Number(inner) = &inner.value {
		Ok(Varible {
			ty: VarType::Number,
			value: VarValue::Number(inner.signum()),
			..Default::default()
		})
	}else {
		Err(("cant use `sign` method on non-number value", "check your logic, change the varible or check your spell").into())
	}
}

/// the `sin` method for `number`
pub fn method_number_sin(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let inner = input.get("self").unwrap();
	if let VarValue::Number(inner) = &inner.value {
		Ok(Varible {
			ty: VarType::Number,
			value: VarValue::Number(inner.sin()),
			..Default::default()
		})
	}else {
		Err(("cant use `sin` method on non-number value", "check your logic, change the varible or check your spell").into())
	}
}

/// the `cos` method for `number`
pub fn method_number_cos(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let inner = input.get("self").unwrap();
	if let VarValue::Number(inner) = &inner.value {
		Ok(Varible {
			ty: VarType::Number,
			value: VarValue::Number(inner.cos()),
			..Default::default()
		})
	}else {
		Err(("cant use `cos` method on non-number value", "check your logic, change the varible or check your spell").into())
	}
}

/// the `tan` method for `number`
pub fn method_number_tan(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let inner = input.get("self").unwrap();
	if let VarValue::Number(inner) = &inner.value {
		Ok(Varible {
			ty: VarType::Number,
			value: VarValue::Number(inner.tan()),
			..Default::default()
		})
	}else {
		Err(("cant use `tan` method on non-number value", "check your logic, change the varible or check your spell").into())
	}
}

/// the `asin` method for `number`
pub fn method_number_asin(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let inner = input.get("self").unwrap();
	if let VarValue::Number(inner) = &inner.value {
		Ok(Varible {
			ty: VarType::Number,
			value: VarValue::Number(inner.asin()),
			..Default::default()
		})
	}else {
		Err(("cant use `asin` method on non-number value", "check your logic, change the varible or check your spell").into())
	}
}

/// the `acos` method for `number`
pub fn method_number_acos(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let inner = input.get("self").unwrap();
	if let VarValue::Number(inner) = &inner.value {
		Ok(Varible {
			ty: VarType::Number,
			value: VarValue::Number(inner.cos()),
			..Default::default()
		})
	}else {
		Err(("cant use `acos` method on non-number value", "check your logic, change the varible or check your spell").into())
	}
}

/// the `tan` method for `number`
pub fn method_number_atan(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let inner = input.get("self").unwrap();
	if let VarValue::Number(inner) = &inner.value {
		Ok(Varible {
			ty: VarType::Number,
			value: VarValue::Number(inner.atan()),
			..Default::default()
		})
	}else {
		Err(("cant use `atan` method on non-number value", "check your logic, change the varible or check your spell").into())
	}
}

/// the `is_int` method for `number`
pub fn method_number_is_int(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let inner = input.get("self").unwrap();
	if let VarValue::Number(inner) = &inner.value {
		Ok(Varible {
			ty: VarType::Bool,
			value: VarValue::Bool(inner.fract() == 0.0),
			..Default::default()
		})
	}else {
		Err(("cant use `is_int` method on non-number value", "check your logic, change the varible or check your spell").into())
	}
}

/// the `is_nan` method for `number`
pub fn method_number_is_nan(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let inner = input.get("self").unwrap();
	if let VarValue::Number(inner) = &inner.value {
		Ok(Varible {
			ty: VarType::Bool,
			value: VarValue::Bool(inner.is_nan()),
			..Default::default()
		})
	}else {
		Err(("cant use `is_nan` method on non-number value", "check your logic, change the varible or check your spell").into())
	}
}

/// the `is_infinite` method for `number`
pub fn method_number_is_infinite(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let inner = input.get("self").unwrap();
	if let VarValue::Number(inner) = &inner.value {
		Ok(Varible {
			ty: VarType::Bool,
			value: VarValue::Bool(inner.is_infinite()),
			..Default::default()
		})
	}else {
		Err(("cant use `is_infinite` method on non-number value", "check your logic, change the varible or check your spell").into())
	}
}

/// the `push` method for `ArrayAny`
pub fn method_array_push(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let push = {
		let push = input.get("input").unwrap();
		if push.ty.is_result() {
			return Err(("cant insert a `Result` value into array", "change the element or check your spell").into())
		}
		push.clone()
	};
	let inner = input.get_mut("self").unwrap();
	if let VarValue::Array(inner) = &mut inner.value {
		if inner.is_empty() {
			println!("{:?}", inner.len());
			inner.push(push.value);
			println!("{:?}", inner.len());
		}else if inner[0].get_type() == push.ty {
  				inner.push(push.value);
  			}else {
  				return Err((format!("expct {}, found: {}", inner[0].get_type(), push.ty), "change the element or check your spell").into())
  			}
		Ok(Varible {
			ty: VarType::None,
			value: VarValue::None,
			..Default::default()
		})
	}else {
		Err(("cant use `push` method on non-array value", "check your logic, change the varible or check your spell").into())
	}
}

/// the `insert` method for `ArrayAny`
pub fn method_array_insert(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let push = {
		let push = input.get("input").unwrap();
		if push.ty.is_result() {
			return Err(("cant insert a `Result` value into array", "check your logic, change the element or check your spell").into())
		}
		push.clone()
	};
	let id = {
		let id = input.get("index").unwrap();
		if let VarValue::Number(inner) = id.value {
			if inner < 0.0 {
				0
			}else {
				inner.round() as usize
			}
		}else {
			unreachable!()
		}
	};
	let inner = input.get_mut("self").unwrap();
	if let VarValue::Array(inner) = &mut inner.value {
		if inner.is_empty() {
			if id == 0 {
				inner.push(push.value);
			}else {
				return Err((format!("the index: `{}` is larger than current len: `{}`", id, inner.len()), "check your logic, change the varible or check your spell").into())
			}
		}else if inner[0].get_type() == push.ty {
  				if id <= inner.len() {
  					inner.insert(id, push.value);
  				}else {
  					return Err((format!("the index: `{}` is larger than current len: `{}`", id, inner.len()), "check your logic, change the varible or check your spell").into())
  				}
  			}else {
  				return Err((format!("expct {}, found: {}", inner[0].get_type(), push.ty), "change the element or check your spell").into())
  			}
		Ok(Varible {
			ty: VarType::None,
			value: VarValue::None,
			..Default::default()
		})
	}else {
		Err(("cant use `insert` method on non-array value", "check your logic, change the varible or check your spell").into())
	}
}

/// the `remove` method for `ArrayAny`
pub fn method_array_remove(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let id = {
		let id = input.get("index").unwrap();
		if let VarValue::Number(inner) = id.value {
			if inner < 0.0 {
				0
			}else {
				inner.round() as usize
			}
		}else {
			unreachable!()
		}
	};
	let inner = input.get_mut("self").unwrap();
	if let VarValue::Array(inner) = &mut inner.value {
		if inner.is_empty() {
			Err((format!("the index: `{}` is larger or equal to current len: `{}`", id, inner.len()), "check your logic, change the varible or check your spell").into())
		}else {
			let value = if id < inner.len() {
				inner.remove(id)
			}else {
				return Err((format!("the index: `{}` is larger or equal to current len: `{}`", id, inner.len()), "check your logic, change the varible or check your spell").into())
			};
			Ok(Varible {
				ty: value.get_type(),
				value,
				..Default::default()
			})
		}
	}else {
		Err(("cant use `remove` method on non-array value", "check your logic, change the varible or check your spell").into())
	}
}

/// the `pop` method for `ArrayAny`
pub fn method_array_pop(input: &mut HashMap<String, Varible>) -> Result<Varible, CustomError> {
	let inner = input.get_mut("self").unwrap();
	if let VarValue::Array(inner) = &mut inner.value {
		if inner.is_empty() {
			Err(("cant pop a zero-len array".to_string(), "check your logic or check your spell").into())
		}else {
			let value = inner.pop().unwrap();
			Ok(Varible {
				ty: value.get_type(),
				value,
				..Default::default()
			})
		}
	}else {
		Err(("cant use `pop` method on non-array value", "check your logic, change the varible or check your spell").into())
	}
}