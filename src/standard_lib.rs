use crate::Macro;
use crate::VisualMachine;
use crate::specific_function::*;
use crate::VarType;
use crate::Function;
use crate::HashMap;
use crate::Interface;

impl VisualMachine {
	/// Basic libraray that didnt contains any other element except caculation related interfaces.
	///
	/// The added methos for num are [`method_number_round`], [`method_number_round_ties_even`], 
	/// [`method_number_is_nan`] and [`method_number_is_infinite`].
	///
	/// The added methods for string are [`method_string_get_slice`] and [`method_len`].
	///
	/// The added methods for array are [`method_len`], [`method_array_push`], [`method_array_insert`], [`method_array_remove`] and [`method_array_pop`].
	///
	/// The added interfaces are [`Interface::any_define()`], [`Interface::add_define()`], [`Interface::sub_define()`], 
	/// [`Interface::mul_define()`], [`Interface::div_define()`], [`Interface::pow_define()`], [`Interface::cmp_define()`], 
	/// [`Interface::eq_define()`], [`Interface::bit_and_define()`], [`Interface::bit_or_define()`], [`Interface::neg_define()`], 
	/// [`Interface::not_define()`] and [`Interface::index_define()`].
	///
	/// Implied [`Interface::any()`] for all types of value, which will automatcally imply it for all type.
	///
	/// Implied [`Interface::eq()`] for all types of value except object.
	///
	/// Implied [`Interface::add()`], [`Interface::sub()`], [`Interface::mul()`], [`Interface::div()`], 
	/// [`Interface::pow()`], [`Interface::neg()`] and [`Interface::cmp()`] for `num`.
	///
	/// Implied [`Interface::bit_and()`], [`Interface::bit_or()`] and [`Interface::not()`] for `bool`.
	///
	/// Implied [`Interface::add()`] for `str`.
	///
	/// Implied [`Interface::index()`] for `Array<Any>`.
	pub fn bacic_lib() -> Self {
		let empty_self = Self::empty();
		let _ = empty_self.regstrate_interface(Interface::any_define());
		let _ = empty_self.regstrate_interface(Interface::add_define());
		let _ = empty_self.regstrate_interface(Interface::sub_define());
		let _ = empty_self.regstrate_interface(Interface::mul_define());
		let _ = empty_self.regstrate_interface(Interface::div_define());
		let _ = empty_self.regstrate_interface(Interface::pow_define());
		let _ = empty_self.regstrate_interface(Interface::cmp_define());
		let _ = empty_self.regstrate_interface(Interface::eq_define());
		let _ = empty_self.regstrate_interface(Interface::bit_and_define());
		let _ = empty_self.regstrate_interface(Interface::bit_or_define());
		let _ = empty_self.regstrate_interface(Interface::neg_define());
		let _ = empty_self.regstrate_interface(Interface::not_define());
		let _ = empty_self.regstrate_interface(Interface::index_define());
		let _ = empty_self.imply_interface(&VarType::None, Interface::any());
		let _ = empty_self.imply_interface(&VarType::Number, Interface::any());
		let _ = empty_self.imply_interface(&VarType::Bool, Interface::any());
		let _ = empty_self.imply_interface(&VarType::String, Interface::any());
		let _ = empty_self.imply_interface(&VarType::ArrayAny, Interface::any());
		let _ = empty_self.imply_interface(&VarType::Number, Interface::add());
		let _ = empty_self.imply_interface(&VarType::String, Interface::add());
		let _ = empty_self.imply_interface(&VarType::Number, Interface::sub());
		let _ = empty_self.imply_interface(&VarType::Number, Interface::mul());
		let _ = empty_self.imply_interface(&VarType::Number, Interface::div());
		let _ = empty_self.imply_interface(&VarType::Number, Interface::pow());
		let _ = empty_self.imply_interface(&VarType::Number, Interface::neg());
		let _ = empty_self.imply_interface(&VarType::Number, Interface::cmp());
		let _ = empty_self.imply_interface(&VarType::None, Interface::eq());
		let _ = empty_self.imply_interface(&VarType::Number, Interface::eq());
		let _ = empty_self.imply_interface(&VarType::Bool, Interface::eq());
		let _ = empty_self.imply_interface(&VarType::String, Interface::eq());
		let _ = empty_self.imply_interface(&VarType::ArrayAny, Interface::eq());
		let _ = empty_self.imply_interface(&VarType::Bool, Interface::bit_and());
		let _ = empty_self.imply_interface(&VarType::Bool, Interface::bit_or());
		let _ = empty_self.imply_interface(&VarType::Bool, Interface::not());
		let _ = empty_self.imply_interface(&VarType::ArrayAny, Interface::index());
		let _ = empty_self.imply_method(&VarType::String, Function::from(method_string_get_slice)
			.name("get_slice")
			.input(vec!(("self".to_string(), VarType::SelfType), ("input".to_string(), VarType::Number), ("input".to_string(), VarType::Number)))
			.output(VarType::String)
		);
		let _ = empty_self.imply_method(&VarType::String, 
			Function::from(method_len)
			.name("len")
			.input(vec!(("self".to_string(), VarType::SelfType)))
			.output(VarType::Number)
		);
		let _ = empty_self.imply_method(&VarType::ArrayAny, 
			Function::from(method_len)
			.name("len")
			.input(vec!(("self".to_string(), VarType::SelfType)))
			.output(VarType::Number)
		);
		let _ = empty_self.imply_method(&VarType::Number, 
			Function::from(method_number_round)
			.name("round")
			.input(vec!(("self".to_string(), VarType::SelfType)))
			.output(VarType::Number)
		);
		let _ = empty_self.imply_method(&VarType::Number, 
			Function::from(method_number_round_ties_even)
			.name("round_ties_even")
			.input(vec!(("self".to_string(), VarType::SelfType)))
			.output(VarType::Number)
		);
		let _ = empty_self.imply_method(&VarType::Number, 
			Function::from(method_number_is_nan)
			.name("is_nan")
			.input(vec!(("self".to_string(), VarType::SelfType)))
			.output(VarType::Bool)
		);
		let _ = empty_self.imply_method(&VarType::Number, 
			Function::from(method_number_is_infinite)
			.name("is_infinite")
			.input(vec!(("self".to_string(), VarType::SelfType)))
			.output(VarType::Bool)
		);
		let _ = empty_self.imply_method(&VarType::ArrayAny, 
			Function::from(method_array_push)
			.name("push")
			.input(vec!(
				("self".to_string(), VarType::SelfType),
				("input".to_string(), VarType::Dynamic("Any".to_string())),
			))
			.output(VarType::None)
		);
		let _ = empty_self.imply_method(&VarType::ArrayAny, 
			Function::from(method_array_insert)
			.name("insert")
			.input(vec!(
				("self".to_string(), VarType::SelfType),
				("index".to_string(), VarType::Number),
				("input".to_string(), VarType::Dynamic("Any".to_string())),
			))
			.output(VarType::None)
		);
		let _ = empty_self.imply_method(&VarType::ArrayAny, 
			Function::from(method_array_remove)
			.name("remove")
			.input(vec!(
				("self".to_string(), VarType::SelfType),
				("index".to_string(), VarType::Number),
			))
			.output(VarType::Dynamic("Any".to_string()))
		);
		let _ = empty_self.imply_method(&VarType::ArrayAny, 
			Function::from(method_array_pop)
			.name("pop")
			.input(vec!(
				("self".to_string(), VarType::SelfType),
			))
			.output(VarType::Dynamic("Any".to_string()))
		);
		empty_self
	}

	/// full standard lib support
	///
	/// Stanard lib contains all function in basic lib.
	///
	/// The newly added functions are [`print()`], [`println()`], [`format()`] and [`compile_error()`].
	///
	/// The newly added marcros are [`macro_println`] and [`macro_print`]
	///
	/// The newly added method for `num` are [`method_number_floor`], [`method_number_ceil`], 
	/// [`method_number_int_part`], [`method_number_fract_part`], [`method_number_abs`], [`method_number_sign`], 
	/// [`method_number_sin`], [`method_number_cos`], [`method_number_tan`], [`method_number_asin`], 
	/// [`method_number_acos`], [`method_number_atan`] and [`method_number_is_int`]
	pub fn standard_lib() -> Self {
		let out = Self::bacic_lib();
		let _ = out.regstrate_function(Function::from(print)
			.name("print")
			.input(vec!(("input".to_string(), VarType::Dynamic("Any".to_string()))))
			.output(VarType::None)
		);
		let _ = out.regstrate_function(Function::from(println)
			.name("println")
			.input(vec!(("input".to_string(), VarType::Dynamic("Any".to_string()))))
			.output(VarType::None)
		);
		let _ = out.regstrate_function(Function::from(format)
			.name("format")
			.input(vec!(("input".to_string(), VarType::Dynamic("Any".to_string()))))
			.output(VarType::String)
		);
		let _ = out.regstrate_function(Function::from(compile_error)
			.name("compile_error")
			.input(
				vec!(
					("description".to_string(), VarType::Dynamic("Any".to_string())), 
					("suggestion".to_string(), VarType::Dynamic("Any".to_string()))
				)
			)
			.output(VarType::None)
		);
		let _ = out.regstrate_macro(Macro::from(macro_println)
			.name("println")
			.input_name("input")
		);
		let _ = out.regstrate_macro(Macro::from(macro_print)
			.name("print")
			.input_name("input")
		);
		let _ = out.imply_method(&VarType::Number, 
			Function::from(method_number_floor)
			.name("floor")
			.input(vec!(("self".to_string(), VarType::SelfType)))
			.output(VarType::Number)
		);
		let _ = out.imply_method(&VarType::Number, 
			Function::from(method_number_ceil)
			.name("ceil")
			.input(vec!(("self".to_string(), VarType::SelfType)))
			.output(VarType::Number)
		);
		let _ = out.imply_method(&VarType::Number, 
			Function::from(method_number_int_part)
			.name("int_part")
			.input(vec!(("self".to_string(), VarType::SelfType)))
			.output(VarType::Number)
		);
		let _ = out.imply_method(&VarType::Number, 
			Function::from(method_number_fract_part)
			.name("fract_part")
			.input(vec!(("self".to_string(), VarType::SelfType)))
			.output(VarType::Number)
		);
		let _ = out.imply_method(&VarType::Number, 
			Function::from(method_number_abs)
			.name("abs")
			.input(vec!(("self".to_string(), VarType::SelfType)))
			.output(VarType::Number)
		);
		let _ = out.imply_method(&VarType::Number, 
			Function::from(method_number_sign)
			.name("sign")
			.input(vec!(("self".to_string(), VarType::SelfType)))
			.output(VarType::Number)
		);
		let _ = out.imply_method(&VarType::Number, 
			Function::from(method_number_sin)
			.name("sin")
			.input(vec!(("self".to_string(), VarType::SelfType)))
			.output(VarType::Number)
		);
		let _ = out.imply_method(&VarType::Number, 
			Function::from(method_number_cos)
			.name("cos")
			.input(vec!(("self".to_string(), VarType::SelfType)))
			.output(VarType::Number)
		);
		let _ = out.imply_method(&VarType::Number, 
			Function::from(method_number_tan)
			.name("tan")
			.input(vec!(("self".to_string(), VarType::SelfType)))
			.output(VarType::Number)
		);
		let _ = out.imply_method(&VarType::Number, 
			Function::from(method_number_asin)
			.name("asin")
			.input(vec!(("self".to_string(), VarType::SelfType)))
			.output(VarType::Number)
		);
		let _ = out.imply_method(&VarType::Number, 
			Function::from(method_number_acos)
			.name("acos")
			.input(vec!(("self".to_string(), VarType::SelfType)))
			.output(VarType::Number)
		);
		let _ = out.imply_method(&VarType::Number, 
			Function::from(method_number_atan)
			.name("atan")
			.input(vec!(("self".to_string(), VarType::SelfType)))
			.output(VarType::Number)
		);
		let _ = out.imply_method(&VarType::Number, 
			Function::from(method_number_is_int)
			.name("is_int")
			.input(vec!(("self".to_string(), VarType::SelfType)))
			.output(VarType::Bool)
		);
		out
	}
}

impl Interface {
	/// the `Any` interface in nablo language
	pub fn any() -> Self {
		let mut function = HashMap::new();
		function.insert("type".into(), Function::from(any_type)
			.name("type")
			.input(vec!(("self".into(), VarType::SelfType)))
			.output(VarType::String)
		);
		function.insert("is_same_type".into(), Function::from(any_is_same_type)
			.name("is_same_type")
			.input(vec!(("self".into(), VarType::SelfType), ("other".into(), VarType::None)))
			.output(VarType::Bool)
		);
		function.insert("downcast_to_other".into(), Function::from(any_downcast_to_other)
			.name("downcast_to_other")
			.input(vec!(("self".into(), VarType::SelfType), ("other".into(), VarType::None)))
			.output(VarType::Result(Box::new(VarType::None), Box::new(VarType::None)))
		);
		Self {
			name: "Any".to_string(),
			function,
		}
	}

	/// the `Any` interface in nablo language, but function is empty
	pub fn any_define() -> Self {
		let mut function = HashMap::new();
		function.insert("type".into(), Function::empty()
			.name("type")
			.input(vec!(("self".into(), VarType::SelfType)))
			.output(VarType::String)
		);
		function.insert("is_same_type".into(), Function::empty()
			.name("is_same_type")
			.input(vec!(("self".into(), VarType::SelfType), ("other".into(), VarType::None)))
			.output(VarType::Bool)
		);
		function.insert("downcast_to_other".into(), Function::empty()
			.name("downcast_to_other")
			.input(vec!(("self".into(), VarType::SelfType), ("other".into(), VarType::None)))
			.output(VarType::Result(Box::new(VarType::None), Box::new(VarType::None)))
		);
		Self {
			name: "Any".to_string(),
			function,
		}
	}

	/// the `Add` interface in nablo language
	pub fn add() -> Self {
		let mut function = HashMap::new();
		function.insert("add".to_string(), Function::from(add_add)
			.name("add")
			.input(vec!(("self".into(), VarType::SelfType), ("rhs".into(), VarType::Dynamic("Any".to_string()))))
			.output(VarType::Dynamic("Any".to_string())));
		Self {
			name: "Add".to_string(),
			function,
		}
	}

	/// the `Add` interface in nablo language, but function is empty
	pub fn add_define() -> Self {
		let mut function = HashMap::new();
		function.insert("add".to_string(), Function::empty()
			.name("add")
			.input(vec!(("self".into(), VarType::SelfType), ("rhs".into(), VarType::Dynamic("Any".to_string()))))
			.output(VarType::Dynamic("Any".to_string())));
		Self {
			name: "Add".to_string(),
			function,
		}
	}

	/// the `Sub` interface in nablo language
	pub fn sub() -> Self {
		let mut function = HashMap::new();
		function.insert("sub".to_string(), Function::from(sub_sub)
			.name("sub")
			.input(vec!(("self".into(), VarType::SelfType), ("rhs".into(), VarType::Dynamic("Any".to_string()))))
			.output(VarType::Dynamic("Any".to_string())));
		Self {
			name: "Sub".to_string(),
			function,
		}
	}

	/// the `Sub` interface in nablo language, but function is empty
	pub fn sub_define() -> Self {
		let mut function = HashMap::new();
		function.insert("sub".to_string(), Function::empty()
			.name("sub")
			.input(vec!(("self".into(), VarType::SelfType), ("rhs".into(), VarType::Dynamic("Any".to_string()))))
			.output(VarType::Dynamic("Any".to_string())));
		Self {
			name: "Sub".to_string(),
			function,
		}
	}

	/// the `Mul` interface in nablo language
	pub fn mul() -> Self {
		let mut function = HashMap::new();
		function.insert("mul".to_string(), Function::from(mul_mul)
			.name("mul")
			.input(vec!(("self".into(), VarType::SelfType), ("rhs".into(), VarType::Dynamic("Any".to_string()))))
			.output(VarType::Dynamic("Any".to_string())));
		Self {
			name: "Mul".to_string(),
			function,
		}
	}

	/// the `Mul` interface in nablo language, but function is empty
	pub fn mul_define() -> Self {
		let mut function = HashMap::new();
		function.insert("mul".to_string(), Function::empty()
			.name("mul")
			.input(vec!(("self".into(), VarType::SelfType), ("rhs".into(), VarType::Dynamic("Any".to_string()))))
			.output(VarType::Dynamic("Any".to_string())));
		Self {
			name: "Mul".to_string(),
			function,
		}
	}

	/// the `Div` interface in nablo language
	pub fn div() -> Self {
		let mut function = HashMap::new();
		function.insert("div".to_string(), Function::from(div_div)
			.name("div")
			.input(vec!(("self".into(), VarType::SelfType), ("rhs".into(), VarType::Dynamic("Any".to_string()))))
			.output(VarType::Dynamic("Any".to_string())));
		Self {
			name: "Div".to_string(),
			function,
		}
	}

	/// the `Div` interface in nablo language, but function is empty
	pub fn div_define() -> Self {
		let mut function = HashMap::new();
		function.insert("div".to_string(), Function::empty()
			.name("div")
			.input(vec!(("self".into(), VarType::SelfType), ("rhs".into(), VarType::Dynamic("Any".to_string()))))
			.output(VarType::Dynamic("Any".to_string())));
		Self {
			name: "Div".to_string(),
			function,
		}
	}

	/// the `Pow` interface in nablo language
	pub fn pow() -> Self {
		let mut function = HashMap::new();
		function.insert("pow".to_string(), Function::from(pow_pow)
			.name("pow")
			.input(vec!(("self".into(), VarType::SelfType), ("rhs".into(), VarType::Dynamic("Any".to_string()))))
			.output(VarType::Dynamic("Any".to_string())));
		Self {
			name: "Pow".to_string(),
			function,
		}
	}

	/// the `Pow` interface in nablo language, but function is empty
	pub fn pow_define() -> Self {
		let mut function = HashMap::new();
		function.insert("pow".to_string(), Function::empty()
			.name("pow")
			.input(vec!(("self".into(), VarType::SelfType), ("rhs".into(), VarType::Dynamic("Any".to_string()))))
			.output(VarType::Dynamic("Any".to_string())));
		Self {
			name: "Pow".to_string(),
			function,
		}
	}

	/// the `Cmp` interface in nablo language, but function is empty
	pub fn cmp() -> Self {
		let mut function = HashMap::new();
		function.insert("large_equal".to_string(), Function::from(cmp_large_equal)
			.name("large_equal")
			.input(vec!(("self".into(), VarType::SelfType), ("rhs".into(), VarType::Dynamic("Any".to_string()))))
			.output(VarType::Bool));
		function.insert("less_equal".to_string(), Function::from(cmp_less_equal)
			.name("less_equal")
			.input(vec!(("self".into(), VarType::SelfType), ("rhs".into(), VarType::Dynamic("Any".to_string()))))
			.output(VarType::Bool));
		function.insert("large".to_string(), Function::from(cmp_large)
			.name("large")
			.input(vec!(("self".into(), VarType::SelfType), ("rhs".into(), VarType::Dynamic("Any".to_string()))))
			.output(VarType::Bool));
		function.insert("less".to_string(), Function::from(cmp_less)
			.name("less")
			.input(vec!(("self".into(), VarType::SelfType), ("rhs".into(), VarType::Dynamic("Any".to_string()))))
			.output(VarType::Bool));
		Self {
			name: "Cmp".to_string(),
			function,
		}
	}

	/// the `Cmp` interface in nablo language, but function is empty
	pub fn cmp_define() -> Self {
		let mut function = HashMap::new();
		function.insert("large_equal".to_string(), Function::empty()
			.name("large_equal")
			.input(vec!(("self".into(), VarType::SelfType), ("rhs".into(), VarType::Dynamic("Any".to_string()))))
			.output(VarType::Bool));
		function.insert("less_equal".to_string(), Function::empty()
			.name("less_equal")
			.input(vec!(("self".into(), VarType::SelfType), ("rhs".into(), VarType::Dynamic("Any".to_string()))))
			.output(VarType::Bool));
		function.insert("large".to_string(), Function::empty()
			.name("large")
			.input(vec!(("self".into(), VarType::SelfType), ("rhs".into(), VarType::Dynamic("Any".to_string()))))
			.output(VarType::Bool));
		function.insert("less".to_string(), Function::empty()
			.name("less")
			.input(vec!(("self".into(), VarType::SelfType), ("rhs".into(), VarType::Dynamic("Any".to_string()))))
			.output(VarType::Bool));
		Self {
			name: "Cmp".to_string(),
			function,
		}
	}

	/// the `Eq` interface in nablo language
	pub fn eq() -> Self {
		let mut function = HashMap::new();
		function.insert("unequal".to_string(), Function::from(eq_unequal)
			.name("unequal")
			.input(vec!(("self".into(), VarType::SelfType), ("rhs".into(), VarType::Dynamic("Any".to_string()))))
			.output(VarType::Bool));
		function.insert("equal".to_string(), Function::from(eq_equal)
			.name("equal")
			.input(vec!(("self".into(), VarType::SelfType), ("rhs".into(), VarType::Dynamic("Any".to_string()))))
			.output(VarType::Bool));
		Self {
			name: "Eq".to_string(),
			function,
		}
	}

	/// the `Eq` interface in nablo language, but function is empty
	pub fn eq_define() -> Self {
		let mut function = HashMap::new();
		function.insert("unequal".to_string(), Function::empty()
			.name("unequal")
			.input(vec!(("self".into(), VarType::SelfType), ("rhs".into(), VarType::Dynamic("Any".to_string()))))
			.output(VarType::Bool));
		function.insert("equal".to_string(), Function::empty()
			.name("equal")
			.input(vec!(("self".into(), VarType::SelfType), ("rhs".into(), VarType::Dynamic("Any".to_string()))))
			.output(VarType::Bool));
		Self {
			name: "Eq".to_string(),
			function,
		}
	}

	/// the `BitAnd` interface in nablo language
	pub fn bit_and() -> Self {
		let mut function = HashMap::new();
		function.insert("and".to_string(), Function::from(bit_and_and)
			.name("and")
			.input(vec!(("self".into(), VarType::SelfType), ("rhs".into(), VarType::Dynamic("Any".to_string()))))
			.output(VarType::Dynamic("Any".to_string())));
		Self {
			name: "BitAnd".to_string(),
			function,
		}
	}

	/// the `BitAnd` interface in nablo language, but function is empty
	pub fn bit_and_define() -> Self {
		let mut function = HashMap::new();
		function.insert("and".to_string(), Function::empty()
			.name("and")
			.input(vec!(("self".into(), VarType::SelfType), ("rhs".into(), VarType::Dynamic("Any".to_string()))))
			.output(VarType::Dynamic("Any".to_string())));
		Self {
			name: "BitAnd".to_string(),
			function,
		}
	}

	/// the `BitOr` interface in nablo language
	pub fn bit_or() -> Self {
		let mut function = HashMap::new();
		function.insert("or".to_string(), Function::from(bit_or_or)
			.name("or")
			.input(vec!(("self".into(), VarType::SelfType), ("rhs".into(), VarType::Dynamic("Any".to_string()))))
			.output(VarType::Dynamic("Any".to_string())));
		Self {
			name: "BitOr".to_string(),
			function,
		}
	}

	/// the `BitOr` interface in nablo language, but function is empty
	pub fn bit_or_define() -> Self {
		let mut function = HashMap::new();
		function.insert("or".to_string(), Function::empty()
			.name("or")
			.input(vec!(("self".into(), VarType::SelfType), ("rhs".into(), VarType::Dynamic("Any".to_string()))))
			.output(VarType::Dynamic("Any".to_string())));
		Self {
			name: "BitOr".to_string(),
			function,
		}
	}

	/// the `Neg` interface in nablo language
	pub fn neg() -> Self {
		let mut function = HashMap::new();
		function.insert("neg".to_string(), Function::from(neg_neg)
			.name("neg")
			.input(vec!(("self".into(), VarType::SelfType)))
			.output(VarType::Dynamic("Any".to_string())));
		Self {
			name: "Neg".to_string(),
			function,
		}
	}

	/// the `Neg` interface in nablo language, but function is empty
	pub fn neg_define() -> Self {
		let mut function = HashMap::new();
		function.insert("neg".to_string(), Function::empty()
			.name("neg")
			.input(vec!(("self".into(), VarType::SelfType)))
			.output(VarType::Dynamic("Any".to_string())));
		Self {
			name: "Neg".to_string(),
			function,
		}
	}

	/// the `Not` interface in nablo language
	pub fn not() -> Self {
		let mut function = HashMap::new();
		function.insert("not".to_string(), Function::from(not_not)
			.name("not")
			.input(vec!(("self".into(), VarType::SelfType), ))
			.output(VarType::Dynamic("Any".to_string())));
		Self {
			name: "Not".to_string(),
			function,
		}
	}

	/// the `Not` interface in nablo language, but function is empty
	pub fn not_define() -> Self {
		let mut function = HashMap::new();
		function.insert("not".to_string(), Function::empty()
			.name("not")
			.input(vec!(("self".into(), VarType::SelfType), ))
			.output(VarType::Dynamic("Any".to_string())));
		Self {
			name: "Not".to_string(),
			function,
		}
	}

	/// the `Index` interface in nablo language
	pub fn index() -> Self {
		let mut function = HashMap::new();
		function.insert("index".to_string(), Function::from(index_index)
			.name("index")
			.input(vec!(("self".into(), VarType::SelfType), ("index".into(), VarType::Dynamic("Any".to_string()))))
			.output(VarType::Dynamic("Any".to_string())));
		Self {
			name: "Index".to_string(),
			function,
		}
	}

	/// the `Index` interface in nablo language, but function is empty
	pub fn index_define() -> Self {
		let mut function = HashMap::new();
		function.insert("index".to_string(), Function::empty()
			.name("index")
			.input(vec!(("self".into(), VarType::SelfType), ("index".into(), VarType::Dynamic("Any".to_string()))))
			.output(VarType::Dynamic("Any".to_string())));
		Self {
			name: "Index".to_string(),
			function,
		}
	}
}