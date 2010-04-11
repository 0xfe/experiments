/*
   Vex SkipList - A full implementation of a C++ templatized SkipList.
   Author: Mohit Cheppudira <mohit@muthanna.com>

   Build with:
    $ g++ -o skiplist skiplist.cc
*/

#include <iostream>
#include <list>
#include <cstdlib>
#include <ctime>
#include <string>

#define VEX_DEBUG true

#define LOG std::cout << "I:" << __FILE__ << ":" << __LINE__ << ": "
#define LOG_DEBUG std::cout << "D:" << __FILE__ << ":" << __LINE__ << ": "
#define LOG_FATAL std::cout << "F:" << __FILE__ << ":" << __LINE__ << ": "

#ifdef VEX_DEBUG
  #define CHECK(expr) { \
    if (!(expr)) { \
      LOG_FATAL << "CHECK failed: " << #expr << std::endl; \
      exit(0); \
    } }

  #define CHECK_EQ(oval, nval) { \
    if (!((oval) == (nval))) { \
      LOG_FATAL << "CHECK failed: " << oval << " != " << nval << std::endl; \
      exit(0); \
    } }
#else
  #define CHECK(expr)
  #define CHECK_EQ(oval, nval)
  #define LOG_DEBUG //
#endif

namespace Vex {

using std::list;
using std::pair;

class TrivialTosser {
 /* This is a coin tosser, you pervs. */

 public:
  TrivialTosser() {};
  ~TrivialTosser() {};

  // Seed the random number generator.
  inline void Seed() {
    srand(time(NULL));
  }

  // Return True for Heads, and False for Tails.
  inline bool TossOff() {
    return rand() >= (RAND_MAX / 2);
  }
};

template<typename KeyType, typename ValueType,
         typename Tosser=TrivialTosser>
class SkipList {
 typedef pair<KeyType, ValueType> PairType;

 struct Element {
   PairType data;
   typename list<Element>::iterator link;
 };

 typedef list<Element> ElemList;

 public:
  SkipList() {
    AddElemList();
    tosser_.Seed();
  };

  ~SkipList() {
    Clear();
  };

  void Clear() {
    for (typename list<ElemList*>::iterator iter = heads_.begin();
         iter != heads_.end(); ++iter) {
      CHECK(*iter != NULL);
      delete *iter;
    }
    heads_.clear();
  }

  ValueType Find(const KeyType& key) {
    bool found = false;
    typename list<ElemList*>::iterator list_iter = heads_.rbegin();

    while (!found) {
      ElemList* list = *list_iter;
      ElemList::iterator last_good_known = list->begin();

      for (typename ElemList::iterator iter = list->begin();
           iter != list->end(); ++j) {
        Element elem = *iter;
        if (elem.data.first == key) {
          return elem.data.second;
        }

        if (elem.data.first > key) {
          break;
        } else {
          last_good_known = iter;
        }
      }
    }
  }

  void Insert(const PairType& elem) {
    bool toss = true;
    typename list<ElemList*>::iterator iter = heads_.begin();
    typename ElemList::iterator last_inserted_iterator = heads_.front()->end();
    ElemList* list = NULL;

    CHECK(IsEnd(last_inserted_iterator));

    while (toss) {
      if (iter == heads_.end()) {
        // If there are no more linked-lists and the coin toss determined
        // that the element should be upgraded, then create a new list.
        list = AddElemList();

        // Since we just added an element to the end of the list, we need
        // to move the iterator back into place.
        iter--;
      } else {
        // A higher list exists. Use it.
        list = *iter;
      }

      CHECK(list != NULL);

      // Create a new element for the list. Make sure that its link pointer
      // points to the same element in a lower list (or the end of the base.)
      Element element;
      element.data = elem;
      element.link = last_inserted_iterator;

      // Insert element into the list and get the iterator for use as a link.
      last_inserted_iterator = InsertSorted(list, element);

      // Advance to the next linked-list.
      ++iter;

      // Toss coin to decide whether we want to upgrade this element.
      toss = tosser_.TossOff();
    }
  }

  ValueType Get() {
    return (heads_.front())->front().data.second;
  }

  void DumpState() {
    int index = 0;
    for (typename list<ElemList*>::iterator iter = heads_.begin();
         iter != heads_.end(); ++iter) {
      CHECK(*iter != NULL);
      ElemList* list = *iter;

      std::cout << "List " << index << ": ";
      for (typename ElemList::iterator j = list->begin();
           j != list->end(); ++j) {
        Element elem = *j;
        std::cout << elem.data.first << " ("
                  << (iter == heads_.begin() ? "End" : (*(elem.link)).data.first)
                  << "), ";
      }

      std::cout << std::endl;
      index++;
    }
  }

 private:
  ElemList* AddElemList() {
    ElemList* list = new ElemList();
    heads_.push_back(list);
    return list;
  }

  bool IsEnd(typename ElemList::iterator iter) {
    return iter == ((heads_.front())->end());
  }

  static typename ElemList::iterator
  InsertSorted(ElemList* list, const Element& elem) {
    bool inserted = false;
    typename ElemList::iterator insertiter;

    for (typename ElemList::iterator iter = list->begin();
         iter != list->end(); ++iter) {
      if ((*iter).data.first > elem.data.first) {
        insertiter = list->insert(iter, elem);
        inserted = true;
        break;
      }
    }

    if (!inserted) insertiter = list->insert(list->end(), elem);
    return insertiter;
  }

  // List of head pointers to underlying linked-lists
  list<ElemList*> heads_;

  // Coin-tosser
  Tosser tosser_;

  // Disallow evil constructors
};

} // namespace Vex

using namespace std;
using namespace Vex;

int main(int argc, char** argv) {
  SkipList<string, int> s;

  s.Insert(make_pair("b", 56));
  CHECK(s.Get() == 56);

  s.Insert(make_pair("c", 57));
  CHECK_EQ(s.Get(), 56);

  s.Insert(make_pair("a", 55));
  CHECK_EQ(s.Get(), 55);

  s.Clear();
  s.DumpState();

  for (int rounds = 0; rounds < 100; rounds++) {
    char key[20];
    sprintf(key, "%s%d", "A", rounds);
    s.Insert(make_pair(key, 56));
    sprintf(key, "%s%d", "B", rounds);
    s.Insert(make_pair(key, 56));
    sprintf(key, "%s%d", "C", rounds);
    s.Insert(make_pair(key, 56));
  }

  LOG << "PASS" << endl;
  s.DumpState();
  return 0;
}

#undef CHECK
#undef CHECK_EQ
#undef LOG
#undef VEX_DEBUG
