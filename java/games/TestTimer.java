import sun.misc.Perf;
import com.sun.j3d.utils.timer.J3DTimer;
import java.util.*;

interface CodeTimeable {
  public void run();
}

abstract class Timer {
  protected long start;
  protected long stop;
  protected String name;

  public Timer(String name) {
    start = 0;
    stop = 0;
    set_name(name);
  }

  public Timer() {
    this("Generic Timer");
  }
    
  public void set_name(String name) {
    this.name = name;
  }

  public String get_name() {
    return name;
  }

  public void start() {
    start = get_counter();
  }

  public void stop() {
    stop = get_counter();
  }

  public long get_time() {
    return stop - start;
  }

  public abstract long get_counter();
}

class SunTimer extends Timer {
  static Perf perf;
  static long freq;

  static {
    perf = Perf.getPerf();
    freq = perf.highResFrequency();
  }

  public SunTimer() {
    super("Sun Timer");
  }

  public long get_counter() {
    return perf.highResCounter();
  }

  public long get_time() {
    return  (stop - start) * 1000000000L / freq;
  }
}

class Java3DTimer extends Timer {
  public Java3DTimer() {
    super("J3D Timer");
  }

  public long get_counter() {
    return J3DTimer.getValue();
  }
}

class SystemTimer extends Timer {
  public SystemTimer() {
    super("System Timer");
  }

  public long get_counter() {
    return System.nanoTime();
  }
}

class TimerSet {
  ArrayList<Timer> timers;

  public TimerSet() {
    timers = new ArrayList<Timer>();
  }

  public void add(Timer t) {
    timers.add(t);
  }

  public void start() {
    for (Timer t : timers) {
      t.start();
    }
  }

  public void stop() {
    for (Timer t : timers) {
      t.stop();
    }
  }

  public void dump_times() {
    for (Timer t : timers) {
      System.out.println(t.get_name() + ": " + t.get_time());
    }
  }
}
      
class CodeTimer {
  public static void TimeThis(CodeTimeable t) {
    TimerSet timerset = new TimerSet();
    timerset.add(new SunTimer());
    timerset.add(new Java3DTimer());
    timerset.add(new SystemTimer());

    timerset.start();
    t.run();
    timerset.stop();

    timerset.dump_times();
  }
}
      
public class TestTimer {
  public static long iterations = 0;

  public static void main(String[] args) {
    if (args.length < 1) {
      System.out.println("Usage: TestTimer [iterations]");
      return;
    }

    iterations = Long.parseLong(args[0]);

    CodeTimer.TimeThis(new CodeTimeable() {
      public void run() {
        for (long x = 0; x < iterations; x++) {}
      }
    });
  }
}
