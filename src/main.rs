use log::LevelFilter;
use nablo_programe::VisualMachine;
use std::fs;

fn main() {
	env_logger::builder()
		.filter(None, LevelFilter::Warn)
		.init();
	let unparsed_file = fs::read_to_string("./src/test.nablo").expect("cannot read file");
	let mut vm = VisualMachine::new();
	if let Err(e) = vm.run_script(&unparsed_file) {
		println!("{}", e);
	};
}