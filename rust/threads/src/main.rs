mod atomics1;
mod thread1;

fn main() {
    thread1::run1();
    thread1::run2();
    thread1::run3();
    thread1::run4();
    thread1::run5();
    thread1::run6();

    atomics1::run();
}
