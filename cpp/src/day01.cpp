#include <iostream>
#include <deque>

int part1() {
  auto c = int { 0 };
  auto next = int { 0 }; 
  auto prev = 1l<<62; 
  while(std::cin >> next) {
    c += next > prev;
    prev = next;
  }
  return c;
}

int part2() {
  std::deque<int> inputs;
  auto c = int { 0 };
  int next;
  while(std::cin >> next) {
    if(inputs.size() > 2) {
      c += next > inputs[0];
      inputs.pop_front();
    }
    inputs.push_back(next);
  }
  return c;
}

int main() {
  std::cout << part2() << '\n';
}


