mod atomics;
mod locks;
mod thread;

fn main() {
    thread::run1();
    thread::run2();
    thread::run3();
    thread::run4();
    thread::run5();
    thread::run6();

    atomics::run();
    locks::run();
}
