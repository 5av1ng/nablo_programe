# Grammer

`nablo` 是一个轻量级的脚本语言，且可以与 rust 交互  
`nablo` 的语法大致就是简化版 rust 但是多一些奇奇怪怪的新关键字  
报错应该是比较人性化的，但是比不上 rust（  
基础的语法如下  

注：除了在单行注释中外，不允许有任何的非 ascii 字符出现  
注：我默认你有相关语言基础  

## Hello World!

所有语言最开始的传统艺能，注意在 `nablo` 里语句末尾的分号是必要的。  

```nablo
// 输出 `Hello Word!`
println("Hello World!");
// 这里用 print 也行，这两个函数的区别是带 ln 的那个会在结尾加一个换行符而后者不会
// 如果你想在程序里面用 print 输出的字符串可以用 format
```
`nablo` 没有主函数的概念，任何你写的代码都会被执行，除了函数和宏，这俩是懒加载的，只会把代码存起来用的时候才运行，但是还是会语法检查的。

## 注释

```nablo
// 这是单行注释
// 单行注释必须在一行的最前面使用
// 不然会报错

// 这样是不行的！
// x = 42; // 宇宙的答案

/*
multi
line
comment
*/
```

## 变量声明

直接赋值变量即可  
变量名必须以下划线或字母开头，之后跟数字字母或下划线
```nablo
x = 40 + 2;
```

使用未声明的变量会报错

```nablo
println(a);
// error: varible `a` not found
//   |
// 1 | println(a);
//   |         ^
//   |
//  => help: try to declare the varible, change the pointed varible or check your spell to aviod current error
```

`nablo` 是一门强类型语言，主要有以下基本类型

```nablo
// 这是 `num` 类型变量
x = 42;
// 这是 `bool` 类型变量
is_ok = true;
// 这是 `str` 类型变量, 字符串必须用 "" 包起来，用 \" 来匹配代指字符串内的引号，用 \\ 指代 \
foo = "foo";
// 这是 `none` 类型变量
_none = none;
```

有一个代表错误的特殊类型，实际上的类型名称叫做 `Result<ty1, ty2>`, 其中 `ty1` 和 `ty2` 是前文提到的任意类型，也可以是后文提到的那个暂时还没说的类型  

```
// `Pass` 代表没有错误
ok = Pass("this is passed!");
// `Block` 代表错误发生
err = Block("error occurred");
```

有数组，没元组，要想要元组的话你得声明一个对象，实际上的类型名称叫做 `Array<ty>`, 其中 `ty` 是前文提到的任意类型，也可以是后文提到的那个暂时还没说的类型  
你可以用 `push` `insert` `pop` `remove` 来向一个数组末尾/指定位置加入元素或向一个数组末尾/指定位置移除一个元素  
用 `len` 获取长度  

```nablo
an_array_len_3 = [1, 2, 3];
// 这不行
// tuple = ("oh", 42, true);
// 这样更不行
// an_array_len_3 = ["oh", 42, true];
an_array_len_3.push(4);
an_array_len_3.remove(1);
// 输出 `[1,3,4]`
println(an_array_len_3);
```

任何实现了 `Index` 接口的都可以像数组那样用数组访问值

```
an_array_len_3 = [1, 2, 3];
string = "ohhh";
// 默认情况下，这可以
// 输出 3
println(an_array_len_3[2]);
// 这不行
// println(string[2]);
// 这也不行，因为 array 就那么长
// println(an_array_len_3[3]);
```

所有变量都是可变的，没有常量声明。  
但是给一个变量赋值时，若右侧的值与左侧的类型不符会报错。  

```nablo
x = 42;
x = "ok";
// error: mismatched value type expect: `num`, find: `str`
//   |
// 2 | x = "ok";
//   | ^
//   |
//  => help: try to change the pointed value or check your spell to aviod current error
```

但是 `none` 可以分配给所有变量, 且若一个变量的值为 `none`, 这个变量将会可以接受任何值

```
x = 42;
// 这是可以的！
x = none;
// 这也是是可以的！
x = "foo";
// 赋了 "foo" 给 `x` 之后接下来再把 42 赋值给 x 会报错
// 这会报错！
// x = 42;
```

支持的运算符有加(+), 减(-), 乘(\*), 除(/), 乘方(^), 与(&), 与(&&), 或(\|), 或(\|\|), 非(!)  
括号当然也是支持的  
你可以为所有实现了 `Add` 接口的类型使用 + 运算，其他依次类推  
支持的比较符号有 相等(==), 不等(!=), 大于(>), 小于(<), 大于等于(>=), 小于等于(<=)  
你可以为所有实现了 `Eq` 接口的类型使用前两者，`Cmp` 的使用后四者  
他们会在编译的时候自动替换为对应的函数  
运算优先级：括号 > 乘方 > 乘除 > 加减 > 与或非 > 比较  
注：`+=` `-=` 或者此类的运算是不存在的！

```nablo
// 默认情况下，这些可以
x = (114 + 514 - 1919) * 810 / 0.618 ^ 0.5;
foo = "oh" + "um";
is_something = !(100 == 200) && (10 > 1 + 200) || ("woc! ice!".is_same_type("a mi yu shuo de dao li")) ;
// 这不行
// foo = "oh" - "o";
```

你可以用来自 `Any` 接口的 `downcast_to_other` 来进行类型转换，这将会把当前量转化为与输入变量相同类型的变量  
这个函数是可能失败的，所以我把代码留到错误处理哪里再说（  

`nablo` 是存在作用域的，所有变量都是局部变量
```nablo
{
	y = 10;
	println(y);
}
// 这样会报错
// println(y);
```

泛型我决定在后文提完接口后再讲

## 控制流

和大多数语言一样，条件语句用 if / else 语法

```nablo
// 输出 `true`, 因为世界并没有崩溃
if 100 > 10 {
	println("true");
}else {
	println("the world breaks");
}
```

循环只有 loop / break / continue, 没有 for 或者 while

```nablo
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
	// 依次输出 `1` `2` `3` `4` `5` `6` `7` `8` `9` `0`
	println(arr[i]);
	i = i + 1;
}
```

## 函数声明

你可以用 `fn`, `func` 或 `function` 这三个关键字声明函数, 因为为什么不（  
函数名的规则和变量一致  
基本语法和 rust 一致  

```nablo
fn greet(input: str) {
	println("hi!" + input);
}

// 未指定返回值的函数实际上会的返回值类型是 `none`
```

你可以用 return 语句返回值，或者直接给 `return` 这个变量赋值  
但是在这里，你赋值的时候不能给 `none` 不然会被当没返回  
注: 递归会慢的离谱而且爆栈概率奇高无比，基本你用递归算个斐波那契数列的第 4 项就爆栈了  
我暂时还没优化，所以你先别急  

```nablo
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
```

## 对象 / 结构体声明

你可以用 `object` 或 `struct` 声明一个对象  
对象的名字就是一个新的类型，以防止你不知道  

```nablo
object Person {
	name: str,
	age: num,
	is_female: bool,
	// `none` 不能在结构体里面声明，因为为什么你要加一个空啊？为什么啊？？？
	// _none: none,
}

struct Data {
	inner: Array<num>,
	// dyn<xxx> 这个是泛型，之后再说
	comment: dyn<Any>,
}
```

你可以用 `impl` 为一个类型实现方法，不只是对象，也包括自带的类型 (除了泛型和 `Result`)  
如何 new 一个对象可以看看下面  
如何访问对象上的方法或者对象的域也可以看看下面  
需要注意的是，如果你要直接调用一个数字上的方法的话得用浮点数，用整数形式会报错  
这边建议你可以直接令一个新变量  

```nablo
object Person {
	name: str,
	age: num,
	is_female: bool,
}

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

// 输出 `Bob`
println(man.name);
// 输出 `36 Alice`
println(woman.age_and_name());
```

## 接口 / 特征和泛型

你可以用 `trait` 或 `interface` 声明一个对象  
然后用 `impl` 语法来为某个类型实现一个接口  
同样的，不只是对象，也包括自带的类型 (除了泛型和 `Result`)  
你可以用 `dyn<TraitName>` 来使用泛型，其中 `TraitName` 是限制的接口名称, `dyn` 是为了告诉编译器这里是个泛型  
你可以用 `Any` 来接受任何值  
然后实际上传入的值的类型会是其原本的值而非 `dyn<TraitName>`  
也就是说如果你知道会传入什么类型的值，你可以直接调那个类型上的方法而不会报错（  
调用接口上的方法和正常调用没区别  

```nablo
trait Foo {
	fn foo(self) -> str {
		// 这里代码你写了也没用，因为默认实现还在开发中
	}
}

interface Baz {
	fn baz(self) -> str {
		// 这里代码你写了也没用，因为默认实现还在开发中
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

println(1.0.baz());
println(1.0.foo());

fn foo(inner: dyn<Foo>) {
	println(inner.foo());
}

foo(1);
foo("aaa");
// 这不行
// foo(true);
```

## 错误处理

用 `ask` 来进行错误处理，多少还是有点奇怪的语法  
大致上和 rust 的 `match` 语法一样  
`ask` 后边跟一个 `Reslut` 类型的值，如果是 `Pass` 就执行上边那串代码  
否则就是下面  
顺便说一下，你可以用 `Any` 接口上的 `downcast_to_other` 方法进行类型转换，转换不了是会报错的  

```nablo
x = "42";
y = "1234567890abcdefghijklmnopqrstuvwxyz";

// 输出 `num`
ask x.downcast_to_other(1) {
    (ok) => {
        x = none;
        x = ok;
        println(ok.type());
    },
    (err) => {
        println(err);
    },
}

// 输出 `42`
println(x);

// 输出 `num`
ask y.downcast_to_other(1) {
    (ok) => {
        y = none;
        y = ok;
        println(ok.type());
    },
    (err) => {
        println(err);
    },
}

// 输出 `NaN`
println(y);

// 输出 `cant convert a `str` value into `bool``
ask x.downcast_to_other(true) {
    (ok) => {
        println(ok.type());
    },
    (err) => {
        println(err);
    },
}

// 输出 `true`
ask "true".downcast_to_other(true) {
    (ok) => {
        println(ok);
    },
    (err) => {
        println(err);
    },
}

// 输出 `false`
ask "false".downcast_to_other(true) {
    (ok) => {
        println(ok);
    },
    (err) => {
        println(err);
    },
}
```

## 宏

本质上是一个特殊的函数，你可以接受一堆代码然后输出代码来执行  
暂时还没有提供给你解析语法的函数  
但是会有的  
你可以用 `macro!` 关键字来声明一个宏  
注意那个 `!` 是不能少的！  

```nablo
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
// 等价于 
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

`print` 和 `println` 也有他们的宏版本

```nablo
// 输出 woc, true, none
println!("woc", true, none);
// 输出 woc, 1, 2, 3
print!("woc", 1, 2, 3);
```

## IO 流

在标准库里面没有提供相关的方法，你可以从 rust 侧自己实现一套，主要是为了安全起见，你也不想用户感染病毒吧（

## 模块

暂时还没有，暂定会用 `use` 关键字，想要模块话开发的话你可以从 rust 侧自己实现一套先