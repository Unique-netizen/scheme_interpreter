/**
 * @file evaluation.cpp
 * @brief Expression evaluation implementation for the Scheme interpreter
 * @author luke36
 * 
 * This file implements evaluation methods for all expression types in the Scheme
 * interpreter. Functions are organized according to ExprType enumeration order
 * from Def.hpp for consistency and maintainability.
 */

#include "value.hpp"
#include "expr.hpp" 
#include "RE.hpp"
#include "syntax.hpp"
#include <cstring>
#include <vector>
#include <map>
#include <climits>

extern std::map<std::string, ExprType> primitives;
extern std::map<std::string, ExprType> reserved_words;

extern Assoc global_env;

Value Fixnum::eval(Assoc &e) { // evaluation of a fixnum
    return IntegerV(n);
}

Value RationalNum::eval(Assoc &e) { // evaluation of a rational number
    if (denominator == 0) {
        throw RuntimeError("Denominator cannot be zero");
    }
    return RationalV(numerator, denominator);
}

Value StringExpr::eval(Assoc &e) { // evaluation of a string
    return StringV(s);
}

Value True::eval(Assoc &e) { // evaluation of #t
    return BooleanV(true);
}

Value False::eval(Assoc &e) { // evaluation of #f
    return BooleanV(false);
}

Value MakeVoid::eval(Assoc &e) { // (void)
    return VoidV();
}

Value Exit::eval(Assoc &e) { // (exit)
    return TerminateV();
}

Value Unary::eval(Assoc &e) { // evaluation of single-operator primitive
    return evalRator(rand->eval(e));
}

Value Binary::eval(Assoc &e) { // evaluation of two-operators primitive
    return evalRator(rand1->eval(e), rand2->eval(e));
}

Value Variadic::eval(Assoc &e) { // evaluation of multi-operator primitive
    //TO COMPLETE THE VARIADIC CLASS
    std::vector<Value> evaled_rands;
    for (int i = 0; i < rands.size(); i++) {
        evaled_rands.push_back(rands[i]->eval(e));
    }
    return evalRator(evaled_rands);
}

//helper function to check whether a string can be converted to number
bool toNumber(const std::string &s) {
    try {
        std::size_t pos;
        std::stod(s, &pos);
        return pos == s.length();
    } catch (...) {
        return false;
    }
}
//helper function to check var's name
void checkName(const std::string &x){
    if (toNumber(x)) {
        throw RuntimeError("Invalid variable name");
    }
    if ((x[0] >= '0' && x[0] <= '9') || x[0] == '.' || x[0] == '@') {
        throw RuntimeError("Invalid variable name");
    }
    for (char c : x) {
        if (c == ' ' || c == '#' || c == '\'' || c == '\"' || c == '`') {
            throw RuntimeError("Invalid variable name");
        }
    }
}
Value Var::eval(Assoc &e) { // evaluation of variable
    //TO identify the invalid variable
    //We request all valid variable just need to be a symbol,you should promise:
    //The first character of a variable name cannot be a digit or any character from the set: {.@}
    //If a string can be recognized as a number, it will be prioritized as a number. For example: 1, -1, +123, .123, +124., 1e-3
    //Variable names can overlap with primitives and reserve_words
    //Variable names can contain any non-whitespace characters except #, ', ", `, but the first character cannot be a digit
    //When a variable is not defined in the current scope, your interpreter should output RuntimeError
    checkName(x);

    Value matched_value = find(x, e);
    if (matched_value.get() == nullptr) {//no binding found
        if (primitives.count(x)) {
             static std::map<ExprType, std::pair<Expr, std::vector<std::string>>> primitive_map = {
                    {E_VOID,     {Expr(new MakeVoid()), {}}},
                    {E_EXIT,     {Expr(new Exit()), {}}},
                    {E_BOOLQ,    {Expr(new IsBoolean(Expr(new Var("parm")))), {"parm"}}},//parameters of procedure is a vector of string(name)
                    {E_INTQ,     {Expr(new IsFixnum(Expr(new Var("parm")))), {"parm"}}},
                    {E_NULLQ,    {Expr(new IsNull(Expr(new Var("parm")))), {"parm"}}},
                    {E_PAIRQ,    {Expr(new IsPair(Expr(new Var("parm")))), {"parm"}}},
                    {E_PROCQ,    {Expr(new IsProcedure(Expr(new Var("parm")))), {"parm"}}},
                    {E_SYMBOLQ,  {Expr(new IsSymbol(Expr(new Var("parm")))), {"parm"}}},
                    {E_LISTQ,    {Expr(new IsList(Expr(new Var("parm")))), {"parm"}}},
                    {E_STRINGQ,  {Expr(new IsString(Expr(new Var("parm")))), {"parm"}}},
                    {E_DISPLAY,  {Expr(new Display(Expr(new Var("parm")))), {"parm"}}},
                    {E_PLUS,     {Expr(new PlusVar({})), {}}},//varnode in apply
                    {E_MINUS,    {Expr(new MinusVar({})), {}}},
                    {E_MUL,      {Expr(new MultVar({})), {}}},
                    {E_DIV,      {Expr(new DivVar({})), {}}},
                    {E_MODULO,   {Expr(new Modulo(Expr(new Var("parm1")), Expr(new Var("parm2")))), {"parm1","parm2"}}},
                    {E_EXPT,     {Expr(new Expt(Expr(new Var("parm1")), Expr(new Var("parm2")))), {"parm1","parm2"}}},
                    {E_EQQ,      {Expr(new EqualVar({})), {}}},
                    {E_LT,       {Expr(new LessVar({})), {}}},
                    {E_LE,       {Expr(new LessEqVar({})), {}}},
                    {E_EQ,       {Expr(new EqualVar({})), {}}},
                    {E_GE,       {Expr(new GreaterEqVar({})), {}}},
                    {E_GT,       {Expr(new GreaterVar({})), {}}},
                    {E_CONS,     {Expr(new Cons(Expr(new Var("parm1")), Expr(new Var("parm2")))), {"parm1","parm2"}}},
                    {E_CAR,      {Expr(new Car(Expr(new Var("parm")))), {"parm"}}},
                    {E_CDR,      {Expr(new Cdr(Expr(new Var("parm")))), {"parm"}}},
                    {E_LIST,     {Expr(new ListFunc({})), {}}},
                    {E_SETCAR,   {Expr(new SetCar(Expr(new Var("parm1")), Expr(new Var("parm2")))), {"parm1","parm2"}}},
                    {E_SETCDR,   {Expr(new SetCdr(Expr(new Var("parm1")), Expr(new Var("parm2")))), {"parm1","parm2"}}},
                    {E_NOT,      {Expr(new Not(Expr(new Var("parm")))), {"parm"}}},
                    {E_AND,      {Expr(new AndVar({})), {}}},
                    {E_OR,       {Expr(new OrVar({})), {}}}
            };
            auto it = primitive_map.find(primitives[x]);
            //to PASS THE parameters correctly;
            //COMPLETE THE CODE WITH THE HINT IN IF SENTENCE WITH CORRECT RETURN VALUE
            if (it != primitive_map.end()) {
                return ProcedureV(it->second.second, it->second.first, e);
            }
      }
      throw RuntimeError("Undefined variable:" +  x);
    }
    return matched_value;
}

Value Plus::evalRator(const Value &rand1, const Value &rand2) { // +
    //To complete the addition logic
    //put dynamic_cast inside if, then will be executed only twice
    if (rand1->v_type == V_INT && rand2->v_type == V_INT){
        int n1 = dynamic_cast<Integer*>(rand1.get())->n;
        int n2 = dynamic_cast<Integer*>(rand2.get())->n;
        int result = n1 + n2;
        return IntegerV(result);
    }else if (rand1->v_type == V_RATIONAL && rand2->v_type == V_RATIONAL){
        auto p1= dynamic_cast<Rational*>(rand1.get());
        auto p2 = dynamic_cast<Rational*>(rand2.get());
        int num = p1->numerator * p2->denominator + p2->numerator * p1->denominator;
        int den = p1->denominator * p2->denominator;
        return RationalV(num, den);
    }else if (rand1->v_type == V_INT && rand2->v_type == V_RATIONAL){
        auto p1 = dynamic_cast<Integer*>(rand1.get());
        auto p2 = dynamic_cast<Rational*>(rand2.get());
        int num = p1->n * p2->denominator + p2->numerator;
        int den = p2->denominator;
        return RationalV(num, den);
    }else if (rand1->v_type == V_RATIONAL && rand2->v_type == V_INT){
        auto p1 = dynamic_cast<Rational*>(rand1.get());
        auto p2 = dynamic_cast<Integer*>(rand2.get());
        int num = p1->numerator + p2->n * p1->denominator;
        int den = p1->denominator;
        return RationalV(num, den);
    }
    throw(RuntimeError("Wrong typename"));
}

Value Minus::evalRator(const Value &rand1, const Value &rand2) { // -
    //To complete the substraction logic
    if (rand1->v_type == V_INT && rand2->v_type == V_INT){
        int n1 = dynamic_cast<Integer*>(rand1.get())->n;
        int n2 = dynamic_cast<Integer*>(rand2.get())->n;
        int result = n1 - n2;
        return IntegerV(result);
    }else if (rand1->v_type == V_RATIONAL && rand2->v_type == V_RATIONAL){
        auto p1 = dynamic_cast<Rational*>(rand1.get());
        auto p2 = dynamic_cast<Rational*>(rand2.get());
        int num = p1->numerator * p2->denominator - p2->numerator * p1->denominator;
        int den = p1->denominator * p2->denominator;
        return RationalV(num, den);
    }else if (rand1->v_type == V_INT && rand2->v_type == V_RATIONAL){
        auto p1 = dynamic_cast<Integer*>(rand1.get());
        auto p2 = dynamic_cast<Rational*>(rand2.get());
        int num = p1->n * p2->denominator - p2->numerator;
        int den = p2->denominator;
        return RationalV(num, den);
    }else if (rand1->v_type == V_RATIONAL && rand2->v_type == V_INT){
        auto p1 = dynamic_cast<Rational*>(rand1.get());
        auto p2 = dynamic_cast<Integer*>(rand2.get());
        int num = p1->numerator - p2->n * p1->denominator;
        int den = p1->denominator;
        return RationalV(num, den);
    }
    throw(RuntimeError("Wrong typename"));
}

Value Mult::evalRator(const Value &rand1, const Value &rand2) { // *
    //To complete the Multiplication logic
    if (rand1->v_type == V_INT && rand2->v_type == V_INT){
        int n1 = dynamic_cast<Integer*>(rand1.get())->n;
        int n2 = dynamic_cast<Integer*>(rand2.get())->n;
        int result = n1 * n2;
        return IntegerV(result);
    }else if (rand1->v_type == V_RATIONAL && rand2->v_type == V_RATIONAL){
        auto p1 = dynamic_cast<Rational*>(rand1.get());
        auto p2 = dynamic_cast<Rational*>(rand2.get());
        int num = p1->numerator * p2->numerator;
        int den = p1->denominator * p2->denominator;
        return RationalV(num, den);
    }else if (rand1->v_type == V_INT && rand2->v_type == V_RATIONAL){
        auto p1 = dynamic_cast<Integer*>(rand1.get());
        auto p2 = dynamic_cast<Rational*>(rand2.get());
        int num = p1->n * p2->numerator;
        int den = p2->denominator;
        return RationalV(num, den);
    }else if (rand1->v_type == V_RATIONAL && rand2->v_type == V_INT){
        auto p1 = dynamic_cast<Rational*>(rand1.get());
        auto p2 = dynamic_cast<Integer*>(rand2.get());
        int num = p1->numerator * p2->n;
        int den = p1->denominator;
        return RationalV(num, den);
    }
    throw(RuntimeError("Wrong typename"));
}

Value Div::evalRator(const Value &rand1, const Value &rand2) { // /
    //To complete the division logic
    if (rand1->v_type == V_INT && rand2->v_type == V_INT){
        int num = dynamic_cast<Integer*>(rand1.get())->n;
        int den = dynamic_cast<Integer*>(rand2.get())->n;
        if (den == 0){
            throw(RuntimeError("Division by zero"));
        }
        if (num % den == 0){
            return IntegerV(num / den);
        }else{
            return RationalV(num, den);
        }
    }else if (rand1->v_type == V_RATIONAL && rand2->v_type == V_RATIONAL){
        auto p1 = dynamic_cast<Rational*>(rand1.get());
        auto p2 = dynamic_cast<Rational*>(rand2.get());
        if (p2->numerator == 0){
            throw(RuntimeError("Division by zero"));
        }
        int num = p1->numerator * p2->denominator;
        int den = p1->denominator * p2->numerator;
        if (num % den == 0){
            return IntegerV(num / den);
        }else{
            return RationalV(num, den);
        }
    }else if (rand1->v_type == V_INT && rand2->v_type == V_RATIONAL){
        auto p1 = dynamic_cast<Integer*>(rand1.get());
        auto p2 = dynamic_cast<Rational*>(rand2.get());
        if (p2->numerator == 0){
            throw(RuntimeError("Division by zero"));
        }
        int num = p1->n * p2->denominator;
        int den = p2->numerator;
        if (num % den == 0){
            return IntegerV(num / den);
        }else{
            return RationalV(num, den);
        }
    }else if (rand1->v_type == V_RATIONAL && rand2->v_type == V_INT){
        auto p1 = dynamic_cast<Rational*>(rand1.get());
        auto p2 = dynamic_cast<Integer*>(rand2.get());
        if (p2->n == 0){
            throw(RuntimeError("Division by zero"));
        }
        int num = p1->numerator;
        int den = p1->denominator * p2->n;
        if (num % den == 0){
            return IntegerV(num / den);
        }else{
            return RationalV(num, den);
        }
    }
    throw(RuntimeError("Wrong typename"));
}

Value Modulo::evalRator(const Value &rand1, const Value &rand2) { // modulo
    if (rand1->v_type == V_INT && rand2->v_type == V_INT) {
        int dividend = dynamic_cast<Integer*>(rand1.get())->n;
        int divisor = dynamic_cast<Integer*>(rand2.get())->n;
        if (divisor == 0) {
            throw(RuntimeError("Division by zero"));
        }
        return IntegerV(dividend % divisor);
    }
    throw(RuntimeError("modulo is only defined for integers"));
}

// Helper function to calculate greatest common divisor//will this be included???and static
static int gcd(int a, int b) {
    if (a < 0) a = -a;
    if (b < 0) b = -b;
    while (b != 0) {
        int temp = b;
        b = a % b;
        a = temp;
    }
    return a;
}
//helper function to transform args into rationals
static std::vector<std::pair<int, int>> toRationals(const std::vector<Value> &args) {
    std::vector<std::pair<int, int>> rationals;
    for (const auto &arg : args) {
        if (arg->v_type == V_INT) {
            int n = dynamic_cast<Integer*>(arg.get())->n;
            rationals.push_back({n, 1});
        }else if (arg->v_type == V_RATIONAL) {
            auto p = dynamic_cast<Rational*>(arg.get());
            rationals.push_back({p->numerator, p->denominator});
        }else {
            throw(RuntimeError("Wrong typename"));
        }
    }
    return rationals;
}
//helper function to do variadic arithmetics
static std::pair<int, int> arithmeticVar(std::pair<int, int> r1, std::pair<int, int> r2, char op) {
    switch(op) {
        case '+': {
            int num = r1.first * r2.second + r2.first * r1.second;
            int den = r1.second * r2.second;
            int g = gcd(num, den);
            return {num / g, den / g};
        }
        case '-': {
            int num = r1.first * r2.second - r2.first * r1.second;
            int den = r1.second * r2.second;
            int g = gcd(num, den);
            return {num / g, den / g};
        }
        case '*': {
            int num = r1.first * r2.first;
            int den = r1.second * r2.second;
            int g = gcd(num, den);
            return {num / g, den / g};
        }
        case '/': {
            if (r2.first == 0) {
                throw(RuntimeError("Division by zero"));
            }
            int num = r1.first * r2.second;
            int den = r1.second * r2.first;
            int g = gcd(num, den);
            return {num / g, den / g};
        }
    }
}
Value PlusVar::evalRator(const std::vector<Value> &args) { // + with multiple args
    //To complete the addition logic
    if (args.empty()) {
        return IntegerV(0);
    }
    std::vector<std::pair<int, int>> rationals = toRationals(args);
    std::pair<int, int> result = rationals[0];
    for (int i = 1; i < rationals.size(); ++i) {
        result = arithmeticVar(result, rationals[i], '+');
    }
    if (result.second == 1) {
        return IntegerV(result.first);
    } else {
        return RationalV(result.first, result.second);
    }
}

Value MinusVar::evalRator(const std::vector<Value> &args) { // - with multiple args
    //To complete the substraction logic
    std::vector<std::pair<int, int>> rationals = toRationals(args);
    std::pair<int, int> result = rationals[0];
    if (rationals.size() == 1){
        result.first = 0 - result.first;
    }
    for (int i = 1; i < rationals.size(); ++i) {
        result = arithmeticVar(result, rationals[i], '-');
    }
    if (result.second == 1) {
        return IntegerV(result.first);
    } else {
        return RationalV(result.first, result.second);
    }
}

Value MultVar::evalRator(const std::vector<Value> &args) { // * with multiple args
    //To complete the multiplication logic
    if (args.empty()) {
        return IntegerV(1);
    }
    std::vector<std::pair<int, int>> rationals = toRationals(args);
    std::pair<int, int> result = rationals[0];
    for (int i = 1; i < rationals.size(); ++i) {
        result = arithmeticVar(result, rationals[i], '*');
    }
    if (result.second == 1) {
        return IntegerV(result.first);
    } else {
        return RationalV(result.first, result.second);
    }
}

Value DivVar::evalRator(const std::vector<Value> &args) { // / with multiple args
    //To complete the divisor logic
    std::vector<std::pair<int, int>> rationals = toRationals(args);
    std::pair<int, int> result = rationals[0];
    if (rationals.size() == 1) {
        if (result.first == 0) {
            throw(RuntimeError("Division by zero"));
        }
        result = {result.second, result.first};
    }
    for (int i = 1; i < rationals.size(); ++i) {
        result = arithmeticVar(result, rationals[i], '/');
    }
    if (result.second == 1) {
        return IntegerV(result.first);
    } else {
        return RationalV(result.first, result.second);
    }
}

Value Expt::evalRator(const Value &rand1, const Value &rand2) { // expt
    if (rand1->v_type == V_INT && rand2->v_type == V_INT) {
        int base = dynamic_cast<Integer*>(rand1.get())->n;
        int exponent = dynamic_cast<Integer*>(rand2.get())->n;
        
        if (exponent < 0) {
            throw(RuntimeError("Negative exponent not supported for integers"));
        }
        if (base == 0 && exponent == 0) {
            throw(RuntimeError("0^0 is undefined"));
        }
        
        long long result = 1;
        long long b = base;
        int exp = exponent;
        
        while (exp > 0) {
            if (exp % 2 == 1) {
                result *= b;
                if (result > INT_MAX || result < INT_MIN) {
                    throw(RuntimeError("Integer overflow in expt"));
                }
            }
            b *= b;
            if (b > INT_MAX || b < INT_MIN) {
                if (exp > 1) {
                    throw(RuntimeError("Integer overflow in expt"));
                }
            }
            exp /= 2;
        }
        
        return IntegerV((int)result);
    }
    throw(RuntimeError("Wrong typename"));
}

//A FUNCTION TO SIMPLIFY THE COMPARISON WITH INTEGER AND RATIONAL NUMBER
int compareNumericValues(const Value &v1, const Value &v2) {
    if (v1->v_type == V_INT && v2->v_type == V_INT) {
        int n1 = dynamic_cast<Integer*>(v1.get())->n;
        int n2 = dynamic_cast<Integer*>(v2.get())->n;
        return (n1 < n2) ? -1 : (n1 > n2) ? 1 : 0;
    }
    else if (v1->v_type == V_RATIONAL && v2->v_type == V_INT) {
        Rational* r1 = dynamic_cast<Rational*>(v1.get());
        int n2 = dynamic_cast<Integer*>(v2.get())->n;
        int left = r1->numerator;
        int right = n2 * r1->denominator;
        return (left < right) ? -1 : (left > right) ? 1 : 0;
    }
    else if (v1->v_type == V_INT && v2->v_type == V_RATIONAL) {
        int n1 = dynamic_cast<Integer*>(v1.get())->n;
        Rational* r2 = dynamic_cast<Rational*>(v2.get());
        int left = n1 * r2->denominator;
        int right = r2->numerator;
        return (left < right) ? -1 : (left > right) ? 1 : 0;
    }
    else if (v1->v_type == V_RATIONAL && v2->v_type == V_RATIONAL) {
        Rational* r1 = dynamic_cast<Rational*>(v1.get());
        Rational* r2 = dynamic_cast<Rational*>(v2.get());
        int left = r1->numerator * r2->denominator;
        int right = r2->numerator * r1->denominator;
        return (left < right) ? -1 : (left > right) ? 1 : 0;
    }
    throw RuntimeError("Wrong typename in numeric comparison");
}

Value Less::evalRator(const Value &rand1, const Value &rand2) { // <
    //To complete the less logic
    int compare = compareNumericValues(rand1, rand2);
    if (compare == -1) {
        return BooleanV(true);
    } else {
        return BooleanV(false);
    }
    throw(RuntimeError("Wrong typename"));
}

Value LessEq::evalRator(const Value &rand1, const Value &rand2) { // <=
    //To complete the lesseq logic
    int compare = compareNumericValues(rand1, rand2);
    if (compare == -1 || compare == 0) {
        return BooleanV(true);
    } else {
        return BooleanV(false);
    }
    throw(RuntimeError("Wrong typename"));
}

Value Equal::evalRator(const Value &rand1, const Value &rand2) { // =
    //To complete the equal logic
    int compare = compareNumericValues(rand1, rand2);
    if (compare == 0) {
        return BooleanV(true);
    } else {
        return BooleanV(false);
    }
    throw(RuntimeError("Wrong typename"));
}

Value GreaterEq::evalRator(const Value &rand1, const Value &rand2) { // >=
    //To complete the greatereq logic
    int compare = compareNumericValues(rand1, rand2);
    if (compare == 1 || compare == 0) {
        return BooleanV(true);
    } else {
        return BooleanV(false);
    }
    throw(RuntimeError("Wrong typename"));
}

Value Greater::evalRator(const Value &rand1, const Value &rand2) { // >
    //To complete the greater logic
    int compare = compareNumericValues(rand1, rand2);
    if (compare == 1) {
        return BooleanV(true);
    } else {
        return BooleanV(false);
    }
    throw(RuntimeError("Wrong typename"));
}

Value LessVar::evalRator(const std::vector<Value> &args) { // < with multiple args
    //To complete the less logic
    for (int i = 0; i < args.size() - 1; i++) {
        int compare = compareNumericValues(args[i], args[i + 1]);
        if (compare != -1) {
            return BooleanV(false);
        }
    }
    return BooleanV(true);
}

Value LessEqVar::evalRator(const std::vector<Value> &args) { // <= with multiple args
    //To complete the lesseq logic
    for (int i = 0; i < args.size() - 1; i++) {
        int compare = compareNumericValues(args[i], args[i + 1]);
        if (compare == 1) {
            return BooleanV(false);
        }
    }
    return BooleanV(true);
}

Value EqualVar::evalRator(const std::vector<Value> &args) { // = with multiple args
    //To complete the equal logic
    for (int i = 0; i < args.size() - 1; i++) {
        int compare = compareNumericValues(args[i], args[i + 1]);
        if (compare != 0) {
            return BooleanV(false);
        }
    }
    return BooleanV(true);
}

Value GreaterEqVar::evalRator(const std::vector<Value> &args) { // >= with multiple args
    //To complete the greatereq logic
    for (int i = 0; i < args.size() - 1; i++) {
        int compare = compareNumericValues(args[i], args[i + 1]);
        if (compare == -1) {
            return BooleanV(false);
        }
    }
    return BooleanV(true);
}

Value GreaterVar::evalRator(const std::vector<Value> &args) { // > with multiple args
    //To complete the greater logic
    for (int i = 0; i < args.size() - 1; i++) {
        int compare = compareNumericValues(args[i], args[i + 1]);
        if (compare != 1) {
            return BooleanV(false);
        }
    }
    return BooleanV(true);
}

Value Cons::evalRator(const Value &rand1, const Value &rand2) { // cons
    //To complete the cons logic
    return PairV(rand1, rand2);
}

Value ListFunc::evalRator(const std::vector<Value> &args) { // list function
    //To complete the list logic
    Value pointer = NullV();
    for (int i = args.size() - 1; i >= 0; i--) {
        pointer = PairV(args[i], pointer);
    }
    return pointer;
}

Value IsList::evalRator(const Value &rand) { // list?
    //To complete the list? logic
    if (rand->v_type == V_NULL) return BooleanV(true);
    if (auto p = dynamic_cast<Pair*>(rand.get())) {
        return evalRator(p->cdr);
    }
    return BooleanV(false);
}

Value Car::evalRator(const Value &rand) { // car
    //To complete the car logic
    if (auto p_pair = dynamic_cast<Pair*>(rand.get())) {
        return p_pair->car;
    }
    throw RuntimeError("Wrong typename");
}

Value Cdr::evalRator(const Value &rand) { // cdr
    //To complete the cdr logic
    if (auto p_pair = dynamic_cast<Pair*>(rand.get())) {
        return p_pair->cdr;
    }
    throw RuntimeError("Wrong typename");
}

Value SetCar::evalRator(const Value &rand1, const Value &rand2) { // set-car!
    //To complete the set-car! logic
    auto p_pair = dynamic_cast<Pair*>(rand1.get());
    if (p_pair == nullptr) throw RuntimeError("Wrong typename");
    p_pair->car = rand2;
    return VoidV();
}

Value SetCdr::evalRator(const Value &rand1, const Value &rand2) { // set-cdr!
   //To complete the set-cdr! logic
   auto p_pair = dynamic_cast<Pair*>(rand1.get());
   if (p_pair == nullptr) throw RuntimeError("Wrong typename");
   p_pair->cdr = rand2;
   return VoidV();
}

Value IsEq::evalRator(const Value &rand1, const Value &rand2) { // eq?
    // 检查类型是否为 Integer
    if (rand1->v_type == V_INT && rand2->v_type == V_INT) {
        return BooleanV((dynamic_cast<Integer*>(rand1.get())->n) == (dynamic_cast<Integer*>(rand2.get())->n));
    }
    // 检查类型是否为 Boolean
    else if (rand1->v_type == V_BOOL && rand2->v_type == V_BOOL) {
        return BooleanV((dynamic_cast<Boolean*>(rand1.get())->b) == (dynamic_cast<Boolean*>(rand2.get())->b));
    }
    // 检查类型是否为 Symbol
    else if (rand1->v_type == V_SYM && rand2->v_type == V_SYM) {
        return BooleanV((dynamic_cast<Symbol*>(rand1.get())->s) == (dynamic_cast<Symbol*>(rand2.get())->s));
    }
    // 检查类型是否为 Null 或 Void
    else if ((rand1->v_type == V_NULL && rand2->v_type == V_NULL) ||
             (rand1->v_type == V_VOID && rand2->v_type == V_VOID)) {
        return BooleanV(true);
    } else {
        return BooleanV(rand1.get() == rand2.get());
    }
}

Value IsBoolean::evalRator(const Value &rand) { // boolean?
    return BooleanV(rand->v_type == V_BOOL);
}

Value IsFixnum::evalRator(const Value &rand) { // number?
    return BooleanV(rand->v_type == V_INT);
}

Value IsNull::evalRator(const Value &rand) { // null?
    return BooleanV(rand->v_type == V_NULL);
}

Value IsPair::evalRator(const Value &rand) { // pair?
    return BooleanV(rand->v_type == V_PAIR);
}

Value IsProcedure::evalRator(const Value &rand) { // procedure?
    return BooleanV(rand->v_type == V_PROC);
}

Value IsSymbol::evalRator(const Value &rand) { // symbol?
    return BooleanV(rand->v_type == V_SYM);
}

Value IsString::evalRator(const Value &rand) { // string?
    return BooleanV(rand->v_type == V_STRING);
}

Value Begin::eval(Assoc &e) {
    //To complete the begin logic
    Value result = VoidV();
    for (int i = 0; i < es.size(); i++) {
        result = es[i]->eval(e);
    }
    return result;
}

Value Quote::eval(Assoc& e) {
    //To complete the quote logic
    if (auto p = dynamic_cast<Number*>(s.get())) {
        return IntegerV(p->n);
    } else if (auto p = dynamic_cast<RationalSyntax*>(s.get())) {
        return RationalV(p->numerator, p->denominator);
    } else if (auto p = dynamic_cast<TrueSyntax*>(s.get())) {
        return BooleanV(true);
    } else if (auto p = dynamic_cast<FalseSyntax*>(s.get())) {
        return BooleanV(false);
    } else if (auto p = dynamic_cast<SymbolSyntax*>(s.get())) {
        return SymbolV(p->s);
    } else if (auto p = dynamic_cast<StringSyntax*>(s.get())) {
        return StringV(p->s);
    } else if (auto p = dynamic_cast<List*>(s.get())) {
        Value pointer = NullV();
        if(p->stxs.size()>=3){
            Syntax dot = (p->stxs)[(p->stxs).size() - 2];
            auto whetherdot = dynamic_cast<SymbolSyntax*>(dot.get());
            if(whetherdot != nullptr && whetherdot->s == "."){
                pointer = Quote((p->stxs)[(p->stxs).size() - 1]).eval(e);
                for (int i = (p->stxs).size() - 3; i >= 0; i--){
                    Syntax d = (p->stxs)[i];
                    auto w = dynamic_cast<SymbolSyntax*>(d.get());
                    if(w != nullptr && w->s == "."){
                        throw RuntimeError("Invalid '.' in quote");
                    }
                    pointer = PairV(Quote((p->stxs)[i]).eval(e), pointer);
                }
                return pointer;
            }
        }
        for (int i = (p->stxs).size() - 1; i >= 0; i--){
            Syntax d = (p->stxs)[i];
            auto w = dynamic_cast<SymbolSyntax*>(d.get());
            if(w != nullptr && w->s == "."){
                throw RuntimeError("Invalid '.' in quote");
            }
            pointer = PairV(Quote((p->stxs)[i]).eval(e), pointer);
        }
        return pointer;
    }
    throw RuntimeError("Wrong typename");
}

Value AndVar::eval(Assoc &e) { // and with short-circuit evaluation
    //To complete the and logic
    Value result = BooleanV(true);
    for (int i = 0; i < rands.size(); i++) {
        result = rands[i]->eval(e);
        if (result->v_type == V_BOOL) {
            if (!(dynamic_cast<Boolean*>(result.get())->b)) {
                return BooleanV(false);
            }
        }
    }
    return result;
}

Value OrVar::eval(Assoc &e) { // or with short-circuit evaluation
    //To complete the or logic
    Value result = BooleanV(false);
    for (int i = 0; i < rands.size(); i++) {
        result = rands[i]->eval(e);
        if (result->v_type == V_BOOL) {
            if (!(dynamic_cast<Boolean*>(result.get())->b)) {
                continue;
            }
        }
        return result;
    }
    return result;
}

Value Not::evalRator(const Value &rand) { // not
    //To complete the not logic
    if (rand->v_type == V_BOOL) {
        if (!(dynamic_cast<Boolean*>(rand.get())->b)) return BooleanV(true);
    }
    return BooleanV(false);
}

Value If::eval(Assoc &e) {
    //To complete the if logic
    auto p = cond->eval(e);
    if (p->v_type == V_BOOL && !(dynamic_cast<Boolean*>(p.get())->b)){
        return alter->eval(e);
    }else{
        return conseq->eval(e);
    }
}

Value Cond::eval(Assoc &env) {
    //To complete the cond logic
    for (const auto& clause : clauses) {
        Value test = clause[0]->eval(env);
        if (test->v_type == V_BOOL && !(dynamic_cast<Boolean*>(test.get())->b)) {
            continue;
        }
        if (clause.size() == 1) return test;
        Value ret = clause[1]->eval(env);
        for (int i = 2; i < clause.size(); i++) {
            ret = clause[i]->eval(env);
        }
        return ret;
    }
    return VoidV();
}

Value Lambda::eval(Assoc &env) { 
    //To complete the lambda logic
    return ProcedureV(x, e, env);
}

Value Apply::eval(Assoc &e) {
    Value r = rator->eval(e);
    if (r->v_type != V_PROC) {throw RuntimeError("Attempt to apply a non-procedure");}

    //TO COMPLETE THE CLOSURE LOGIC
    Procedure* clos_ptr = dynamic_cast<Procedure*>(r.get());
    
    //TO COMPLETE THE ARGUMENT PARSER LOGIC
    std::vector<Value> args;
    for (int i = 0; i < rand.size(); i++) {
        args.push_back(rand[i]->eval(e));
    }
    if (auto varNode = dynamic_cast<Variadic*>(clos_ptr->e.get())) {
        //expr is variadic, which doesn't have certain number of parameters
        return varNode->evalRator(args);
    }
    if (args.size() != clos_ptr->parameters.size()) throw RuntimeError("Wrong number of arguments");
    
    //TO COMPLETE THE PARAMETERS' ENVIRONMENT LOGIC
    for (auto i = clos_ptr->env; i.get() != nullptr; i = i->next) {
        Value v = find(i->x, global_env);
        if(v.get() != nullptr){
            modify(i->x, v, clos_ptr->env);
        }
    }

    Assoc param_env = clos_ptr->env;
    for (int i = 0; i < args.size(); i++){
        param_env = extend(clos_ptr->parameters[i], args[i], param_env);
    }

    while(true){
        try{
            return clos_ptr->e->eval(param_env);
        }catch(const RuntimeError& error){
            auto p_rator = dynamic_cast<Var*>(rator.get());
            if(!p_rator) throw;
            Value v_rator = find(p_rator->x, global_env);
            if(!v_rator.get()) throw;
            std::string message = error.message();
            const std::string prefix = "Undefined variable:";
            if (message.find(prefix) != 0) throw;
            std::string after = message.substr(prefix.size());
            Value found = find(after, global_env);
            if(!found.get()) throw;
            param_env = extend(after, found, param_env);
        }
    }

}

Value Define::eval(Assoc &env) {
    checkName(var);
    env = extend(var, Value(nullptr), env);
    Value v = e->eval(env);
    modify(var, v, env);
    return VoidV();
}

Value Let::eval(Assoc &env) {
    //To complete the let logic
    //create new env
    Assoc let_env = env;
    for (int i = 0; i < bind.size(); i++){
        checkName(bind[i].first);
        let_env = extend(bind[i].first, bind[i].second->eval(env), let_env);
    }
    
    return body->eval(let_env);
}

Value Letrec::eval(Assoc &env) {
    //To complete the letrec logic
    Assoc env1 = env;
    for (int i = 0; i < bind.size(); i++) {
        checkName(bind[i].first);
        env1 = extend(bind[i].first, Value(nullptr), env1);
    }
    //if we modify env1, later will know the former(already evaled)
    Assoc env2 = env;
    for (int i = 0; i < bind.size(); i++) {
        env2 = extend(bind[i].first, bind[i].second->eval(env1), env2);
    }
    //modify env1 in case of procedure
    for (int i = 0; i < bind.size(); i++) {
        modify(bind[i].first, find(bind[i].first, env2), env1);
    }
    return body->eval(env2);
}

Value Set::eval(Assoc &env) {
    //To complete the set logic
    if (find(var, env).get() == nullptr) {
        throw RuntimeError("Unbound variable in set!");
    }
    modify(var, e->eval(env), env);
    return VoidV();
}

Value Display::evalRator(const Value &rand) { // display function
    if (rand->v_type == V_STRING) {
        String* str_ptr = dynamic_cast<String*>(rand.get());
        std::cout << str_ptr->s;
    } else {
        rand->show(std::cout);
    }
    
    return VoidV();
}
