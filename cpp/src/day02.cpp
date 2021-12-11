#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <numeric>

enum class CommandType {
  Forward, Up, Down
};

struct Command {
  CommandType command;
  int stepSize;
};

std::vector<Command> readCommands() {
  auto res = std::vector<Command> {};
  std::string line;
  while(std::getline(std::cin, line)) {
    auto ss = std::istringstream(line);
    auto i = int {};
    auto command = std::string {};
    ss >> command >> i;
    if (command == "forward") {
      res.push_back(Command { CommandType::Forward, i });
    } else if (command == "down") {
      res.push_back(Command { CommandType::Down, i });
    } else { // down
      res.push_back(Command { CommandType::Up, i });
    }
  }
  return res;
}

struct Pos2 {
  int h;
  int p;
};



int main() {
  auto commands = readCommands();
  auto combine = [](const Pos2& p, Command c) -> Pos2 {
    switch (c.command) {
      // using CommandType;
      case CommandType::Forward:
        return Pos2 { h, p.p + c.stepSize };
        return p;
      case CommandType::Down:
        p.h += c.stepSize;
        return p;
      case CommandType::Up:
        p.h -= c.stepSize;
        return p;
    };
  };
  Pos2 p0;
  auto res = std::accumulate(commands.begin(), commands.end(), p0, combine);
  return 0;
}
