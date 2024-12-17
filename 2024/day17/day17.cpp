#include <iostream>
#include <fstream>
#include <string>
#include <vector>

struct MiniComputer;

struct Operand {
    int v;

    Operand(int i): v{i} {}
    Operand(const Operand& o): v{o.v} {}
    Operand(Operand&& o): v{o.v} {o.v = 0;}
    virtual ~Operand() {}

    Operand& operator=(const Operand& o) {
        if (this == &o) return *this;
        v = o.v;
        return *this;
    }

    Operand& operator=(Operand&& o) {
        if (this == &o) return *this;
        v = o.v;
        o.v = 0;
        return *this;
    }

    virtual long value(const MiniComputer&) const = 0;
};

struct Instruction {
    Operand* operand;

    Instruction(Operand* op): operand{op} {}
    Instruction(const Instruction& i): operand{i.operand} {}
    Instruction(Instruction&& i): operand{i.operand} {i.operand = nullptr;}
    virtual ~Instruction() {delete operand;}

    Instruction& operator=(const Instruction& i) {
        if (this == &i) return *this;
        delete operand;
        operand = i.operand;
        return *this;
    }

    Instruction& operator=(Instruction&& i) {
        if (this == &i) return *this;
        delete operand;
        operand = i.operand;
        i.operand = nullptr;
        return *this;
    }

    virtual void exec(MiniComputer&) const = 0;
    virtual void print() const = 0;
};

struct MiniComputer {
    uint64_t reg_a;
    uint64_t reg_b;
    uint64_t reg_c;
    long pc;
    std::vector<Instruction*> program;
    std::vector<int> initial_tape;
    std::vector<int> output;

    MiniComputer(uint64_t a, uint64_t b, uint64_t c, std::vector<int> it, std::vector<Instruction*> instructions): reg_a{a}, reg_b{b}, reg_c{c}, pc{0}, initial_tape{it}, program{instructions}, output{} {}

    void run() {
        while (pc < program.size()) {
            program[pc]->exec(*this);
        }

        for (int i: output) {
            std::cout << i << ",";
        }
        std::cout << std::endl;
    }

    uint64_t find_initial_register() {
        uint64_t cur_val{1};
        while (true) {
            reset();
            reg_a = cur_val;

            while (pc < program.size()) {
                program[pc]->exec(*this);
                
                if (output.size() > initial_tape.size() || output.size() > 0 && output.back() != initial_tape[output.size() - 1]) {
                    // output can't match the initial tape
                    break;
                }
            }

            if (output.size() == initial_tape.size()) {
                // If we get here, the output matches the inital tape on every position
                return cur_val;
            } else {
                cur_val++;
            }
        }
    }

    void print_program() {
        for (Instruction* i: program) {
            i->print();
            std::cout << std::endl;
        }
    }

    void reset() {
        pc = 0;
        output.clear();
    }
};

struct LiteralOp: Operand {
    LiteralOp(int i): Operand{i} {}

    long value(const MiniComputer&) const override {
        return v;
    }
};

struct ComboOp: Operand {
    ComboOp(int i): Operand{i} {}

    long value(const MiniComputer& computer) const override {
        if (v <= 3) return v;
        else switch (v) {
            case 4: return computer.reg_a;
            case 5: return computer.reg_b;
            case 6: return computer.reg_c;
            case 7: throw new std::runtime_error("Invalid operand (reserved)");
            default: throw new std::runtime_error("Invalid operand");
        }
    }
};

struct ADV: Instruction {
    ADV(int i): Instruction{new ComboOp{i}} {}
        

    void exec(MiniComputer& computer) const override {
        computer.reg_a = computer.reg_a / pow(2, operand->value(computer));
        computer.pc += 1;
    }

    void print() const override {
        std::cout << "ADV(" << operand->v << ")";
    }
};

struct BXL: Instruction {
    BXL(int i): Instruction{new LiteralOp{i}} {}

    void exec(MiniComputer& computer) const override {
        computer.reg_b = computer.reg_b ^ operand->value(computer);
        computer.pc += 1;
    }

    void print() const override {
        std::cout << "BXL(" << operand->v << ")";
    }
};

struct BST: Instruction {
    BST(int i): Instruction{new ComboOp{i}} {}

    void exec(MiniComputer& computer) const override {
        computer.reg_b = operand->value(computer) % 8;
        computer.pc += 1;
    }

    void print() const override {
        std::cout << "BST(" << operand->v << ")";
    }
};

// Both example program and actual input only have a JNZ instruction at the very end, jumping back to the beginning
// If part 2 messes with the tape, this will need to be reworked, potentially quite significantly
struct JNZ: Instruction {
    JNZ(int i): Instruction{new LiteralOp{i}} {}

    void exec(MiniComputer& computer) const override {
        if (computer.reg_a != 0) {
            computer.pc = operand->value(computer);
        } else {
            computer.pc += 1;
        }
    }

    void print() const override {
        std::cout << "JNZ(" << operand->v << ")";
    }
};

struct BXC: Instruction {
    BXC(int i): Instruction{new LiteralOp{i}} {}

    void exec(MiniComputer& computer) const override {
        computer.reg_b = computer.reg_b ^ computer.reg_c;
        computer.pc += 1;
    }

    void print() const override {
        std::cout << "BXC(" << operand->v << ")";
    }
};

struct OUT: Instruction {
    OUT(int i): Instruction{new ComboOp{i}} {}

    void exec(MiniComputer& computer) const override {
        computer.output.push_back(operand->value(computer) % 8);
        computer.pc += 1;
    }

    void print() const override {
        std::cout << "OUT(" << operand->v << ")";
    }
};

struct BDV: Instruction {
    BDV(int i): Instruction{new ComboOp{i}} {}

    void exec(MiniComputer& computer) const override {
        computer.reg_b = computer.reg_a / pow(2, operand->value(computer));
        computer.pc += 1;
    }

    void print() const override {
        std::cout << "BDV(" << operand->v << ")";
    }
};

struct CDV: Instruction {
    CDV(int i): Instruction{new ComboOp{i}} {}

    void exec(MiniComputer& computer) const override {
        computer.reg_c = computer.reg_a / pow(2, operand->value(computer));
        computer.pc += 1;
    }

    void print() const override {
        std::cout << "CDV(" << operand->v << ")";
    }
};

Instruction* parse_instruction(int opcode, int operand) {
    switch (opcode) {
        case 0: return new ADV(operand);
        case 1: return new BXL(operand);
        case 2: return new BST(operand);
        case 3: return new JNZ(operand);
        case 4: return new BXC(operand);
        case 5: return new OUT(operand);
        case 6: return new BDV(operand);
        case 7: return new CDV(operand);
        default: throw new std::runtime_error("Invalid opcode");
    }
}

MiniComputer parse_input(const std::string& filename) {
    std::ifstream file{filename};
    long a, b, c;
    std::vector<int> tape;
    std::vector<Instruction*> instructions;

    while (!isdigit(file.get())) {}
    file.unget();
    file >> a;

    while (!isdigit(file.get())) {}
    file.unget();
    file >> b;

    while (!isdigit(file.get())) {}
    file.unget();
    file >> c;

    while (!isdigit(file.get())) {}
    file.unget();

    while (isdigit(file.peek())) {
        int opcode, operand;
        file >> opcode;
        file.ignore(1);
        file >> operand;
        tape.push_back(opcode);
        tape.push_back(operand);
        instructions.push_back(parse_instruction(opcode, operand));
        file.ignore(1);
    }

    return MiniComputer{a, b, c, tape, instructions};
}

int main() {
    std::string filename;
    std::cin >> filename;

    MiniComputer computer = parse_input(filename);
    computer.print_program();   

    std::cout << "Part 1: ";
    computer.run();

    std::cout << "Part 2: ";
    long p2 = computer.find_initial_register();
    std::cout << p2 << std::endl;

    return 0;
}