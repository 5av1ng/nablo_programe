# Grammer

`nablo_language` is a lightweight script language, can be used by rust

you can learn basic grammer as follows

Note: `nablo_language` dont allow non-ascii letters.

```nablo
// ok

/*
multi
line
oh
*/
```

```nablo
x = 40 + 2;
is_int = x.is_int();
something = "foo";
an_array_len_3 = [1, 2, 3];
_none = none;
ok = Pass("this is passed!");
err = Block("error occurred");

// this will cause an error
// x = "ok";
// while this not, if a varible is `none`, it can be assigned to any other types of values 
_none = x;
// after that this is not allowed;
// _none = "oh";

// any value can be set to none
x = none;
// after that this will be allowed
x = "ok";
// all available caculators are Add(+), Sub(-), Multiply(*), Divide(/), Power(^), And(&), And(&&), Or(|), Or(||), Not(!)
// all available compartors are Same(==), NotSame(!=), Large(>), Less(<), LargeEqual(>=), LessEqual(<=)
// there's no linkers like `+=` `-=`! 

{
	y = 10;
	println(y);
}
// this will cause an error
// println(y);
```

```nablo
// `fn`, `func` and `function` can be used to declare function
// recursion will be extrmely slow, and have a vary high chance to cause stack overflow
fn greet(input: str) {
	println("hi!" + input);
}

// you can use return statement or just assign a value to `return` to return a value
func foo() -> str {
	return "foo";
}

function may_fail(input: num) -> Result<num, str> {
	if ((input.round() / 2).is_int()) {
		return = Pass(input.round() / 2);
	}else {
		return = Block("error occurred");
	}
}

// prints `none`
println(greet(foo()));
println(may_fail(9));
println(may_fail(10));
```

```nablo
// both `object` and `struct` can be used to declare an object
object Person {
	name: str,
	age: num,
	is_female: bool,
	// `none` is not a type 
	// _none: none,
}

struct Data {
	inner: Array<num>,
}

// use `impl` to imply method
impl Person {
	fn new_empty() -> Self {
		return Self {
			name: "",
			age: 0,
			is_female: false,
		};
	}

	fn age_and_name(self) -> str {
		return format(self.age) + " " + self.name;
	}
}


man = Person {
	name: "Bob",
	age: 42,
	is_female: false,
};

woman = Person {
	name: "Alice",
	age: 36,
	is_female: true,
};

empty = Person::new_empty();

println(man.name);
println(woman.age_and_name());
```

```nablo
if 100 > 10 {
	println("true");
}else {
	println("the world breaks");
}

// there's no while or for statement :(
arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 0];
i = 0;
loop {
	if i + 1 > arr.len() {
		break;
	}
	if (i / 2).is_int() {
		i = i + 1;
		continue;
	}
	// panics if i >= arr.len()
	println(arr[i]);
	i = i + 1;
}
```

```nablo
// use `ask` for error handling

fn may_fail(input: num) -> Result<num, str> {
	if ((input.round() / 2).is_int()) {
		return = Pass(input.round() / 2);
	}else {
		return = Block("error occurred");
	}
}

i = 9;
out = 0;

ask may_fail(i) {
    (ok) => {
        println(format(i) + " is divisible by 2! the value is " + format(ok));
        out = ok;
    },
    (err) => {
        println(format(i) + " is not divisible by 2!");
    },
}

i = i + 1;

ask may_fail(i) {
    (ok) => {
        println(format(i) + " is divisible by 2! the value is " + format(ok));
        out = ok;
    },
    (err) => {
        println(format(i) + " is not divisible by 2!");
    },
}
```

```nablo
// use `trait` or `interface` to declare a interface
trait Foo {
	fn foo(self) -> str {
		// the code inside will be ignored
	}
}

interface Baz {
	fn baz(self) -> str {
		// the code inside will be ignored
	}
}

impl Foo for num {
	fn foo(self) -> str {
		return "foo";
	}
}

impl Foo for str {
	fn foo(self) -> str {
		return "foo";
	}
}

impl Baz for num {
	fn baz(self) -> str {
		return "baz";
	}
}

// note: `1.bar()` will cause an error!
// you need to use 1.0 instead
// or use varible

println(1.0.baz());
println(1.0.foo());

// this function recive all types of value as long as that type have implied `Foo`
fn foo(inner: dyn<Foo>) {
	println(inner.foo());
}

foo(1);
foo("aaa");
// this will cause an error
// foo(true);

// every type have implied `Any` automatically, therefore you can use `dyn<Any>` to recevie value of all types.
```

```nablo
// macro is a special function recives the code input and output code
fn foo(input: num) -> str {
	return "foo " + format(input);
}

macro! foo!(code) {
	i = 0;
	out = "";
	loop {
		if i + 1 > code.len() {
			break;
		}
		out = out + "println(foo(" + code[i] + "));";
		i = i + 1;
	}
	return = out;
}

foo!(1, 2, 3, 4, 5, 6, 7, 8 ,9);
// is same as 
// println(foo(1));
// println(foo(2));
// println(foo(3));
// println(foo(4));
// println(foo(5));
// println(foo(6));
// println(foo(7));
// println(foo(8));
// println(foo(9));
```