#include <time.h>

#include <iostream>
#include <sstream>
#include <string>
#include <vector>

// Polluting namespaces is evil.
using namespace std;

/*
 Graph memory access time for identifying memory architecture.
 Mohit Cheppudira <mohit@muthanna.com>

 Returns Google Charts API links which generate charts on the browser.

 Build with:
 $ g++ cache_size.cc -lrt
*/

// Globals are evil.
const string chart_template_prefix =
"http://chart.apis.google.com/chart?chxt=y,x&chs=500x225&cht=lxy&chco=3D7930"
"&chd=t:";
const string chart_template_postfix =
"&chg=14.3,-1,1,1&chls=2,4,0&chm=B,C5D4B5BB,0,0,0&chtt=Access+Time";

template<class T>
class Stats {
 public:
  Stats() {}

  void add(long index, T item) {
    index_.push_back(index);
    data_.push_back(item);
  }

  T mean() const {
    T sum = 0;
    for (int i = 0; i != data_.size(); i++) {
      sum += data_[i];
    }

    return (sum / data_.size());
  }

  const string chart_link() {
    if (data_.size() == 0) return "";
    stringstream data;
    stringstream index_str;

    data << data_[0];
    index_str << index_[0];

    T min = data_[0];
    T max = data_[0];

    for (int i = 1; i != data_.size(); i++) {
      data << "," << data_[i];
      index_str << "," << index_[i];
      if (data_[i] > max) max = data_[i];
      if (data_[i] < min) min = data_[i];
    }

    long min_i = index_[0];
    long max_i = index_[index_.size() - 1];

    stringstream range;
    range << "&chxr=0," << min << "," << max << "|1," << min_i << "," << max_i;
    range << "&chds=" << min << "," << max << "," << min_i << "," << max_i;

    return chart_template_prefix + data.str() + "|" + index_str.str() +
           range.str() + chart_template_postfix;
  }

 private:
  long size_;
  vector<long> index_;
  vector<T> data_;
};

// Raw function pointers are evil.
typedef void (*loop_func)(long, long);

// Time the loop function and return the duration in ns
static long long time_this(loop_func f, long size, long iterations) {
  cout << "Timing with size: " << size << " and iterations "
       << iterations << endl;
  timespec ts;
  clock_settime(CLOCK_PROCESS_CPUTIME_ID, &ts);
  (*f)(size, iterations);
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts);
  cout << "Time taken: " << ts.tv_sec << "s "
       <<  ts.tv_nsec << "ns" << endl;

  return (ts.tv_sec * 1000000000) + ts.tv_nsec;
}

static void simple_loop(long size, long iterations) {
  int a[size];

  for (int y = 0; y < size; ++y) {
    a[y] = 25;
  }

  for (int x = 0; x < iterations; ++x) {
    for (int y = 0; y < size; ++y) {
      int j = a[y];
      j++; // do something with j so compiler doesn't throw it away
    }
  }
}

int main(int argc, char** argv) {
  long long diff = 0;
  long long diff_prime = 0;

  Stats<long long> elapsed_stats;
  Stats<long long> diff_stats;

  for (int i = 50; i < 70; ++i) {
    long long elapsed = time_this(simple_loop, i, 1000000);
    cout << "Diff: " << elapsed - diff << endl;
    elapsed_stats.add(i, elapsed);
    diff_stats.add(i, elapsed - diff);
    diff = elapsed;
  }

  cout << "Time chart: " << elapsed_stats.chart_link() << endl;
  cout << "Diff chart: " << diff_stats.chart_link() << endl;
}
