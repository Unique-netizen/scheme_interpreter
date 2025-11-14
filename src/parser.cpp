/**
 * @file parser.cpp
 * @brief Parsing implementation for Scheme syntax tree to expression tree conversion
 * 
 * This file implements the parsing logic that converts syntax trees into
 * expression trees that can be evaluated.
 * primitive operations, and function applications.
 */

#include "RE.hpp"
#include "Def.hpp"
#include "syntax.hpp"
#include "value.hpp"
#include "expr.hpp"
#include <map>
#include <string>
#include <iostream>

#define mp make_pair
using std::string;
using std::vector;
using std::pair;

extern std::map<std::string, ExprType> primitives;
extern std::map<std::string, ExprType> reserved_words;

/**
 * @brief Default parse method (should be overridden by subclasses)
 */
Expr Syntax::parse(Assoc &env) {
    throw RuntimeError("Unimplemented parse method");
}

Expr Number::parse(Assoc &env) {
    return Expr(new Fixnum(n));
}

Expr RationalSyntax::parse(Assoc &env) {//may need: simplify(in constructor); check 0. 
    //complete the rational parser
    if (denominator == 0) {
        throw RuntimeError("Denominator cannot be zero");
    }
    return Expr(new RationalNum(numerator, denominator));
}

Expr SymbolSyntax::parse(Assoc &env) {
    return Expr(new Var(s));
}

Expr StringSyntax::parse(Assoc &env) {
    return Expr(new StringExpr(s));
}

Expr TrueSyntax::parse(Assoc &env) {
    return Expr(new True());
}

Expr FalseSyntax::parse(Assoc &env) {
    return Expr(new False());
}

Expr List::parse(Assoc &env) {
    if (stxs.empty()) {
        return Expr(new Quote(Syntax(new List())));
    }

    //TODO: check if the first element is a symbol
    //If not, use Apply function to package to a closure;
    //If so, find whether it's a variable or a keyword;
    SymbolSyntax *id = dynamic_cast<SymbolSyntax*>(stxs[0].get());
    if (id == nullptr) {//dynamic cast failed
        //TODO: TO COMPLETE THE LOGIC
    }else{
    string op = id->s;
    if (find(op, env).get() != nullptr) {
        //TODO: TO COMPLETE THE PARAMETER PARSER LOGIC
        
    }
    if (primitives.count(op) != 0) {//whether in primitive map//here we need: expt;cons, car, ...; and so many...
        vector<Expr> parameters;
        //TODO: TO COMPLETE THE PARAMETER PARSER LOGIC
        for (int i = 1; i < stxs.size(); i++){//call the corresponding parse?
            parameters.push_back(stxs[i]->parse(env));//for and or, shouldn't throw, do parse shouldn't check
        }//so rational should check and division shouldn't check before evaluation?
        //other types of illegal?let's see OJ
        ExprType op_type = primitives[op];
        if (op_type == E_PLUS) {
            if (parameters.size() == 2) {
                return Expr(new Plus(parameters[0], parameters[1])); 
            } else {
                return Expr(new PlusVar(parameters));
            }
        } else if (op_type == E_MINUS) {
            //TO COMPLETE THE LOGIC
            if (parameters.size() == 0) {
                throw RuntimeError("Wrong number of arguments for -");
            }
            if (parameters.size() == 2) {
                return Expr(new Minus(parameters[0], parameters[1])); 
            } else {
                return Expr(new MinusVar(parameters));
            }
        } else if (op_type == E_MUL) {
            //TO COMPLETE THE LOGIC
            if (parameters.size() == 2) {
                return Expr(new Mult(parameters[0], parameters[1])); 
            } else {
                return Expr(new MultVar(parameters));
            }
        } else if (op_type == E_DIV) {
            //TO COMPLETE THE LOGIC
            if (parameters.size() == 0) {
                throw RuntimeError("Wrong number of arguments for /");
            }
            if (parameters.size() == 2) {
                return Expr(new Div(parameters[0], parameters[1])); 
            } else {
                return Expr(new DivVar(parameters));
            }
        } else if (op_type == E_MODULO) {
            if (parameters.size() != 2) {
                throw RuntimeError("Wrong number of arguments for modulo");
            }
            return Expr(new Modulo(parameters[0], parameters[1]));
        } else if (op_type == E_EXPT) {
            if (parameters.size() == 2){
                return Expr(new Expt(parameters[0], parameters[1])); 
            } else {
                throw RuntimeError("Wrong number of arguments for expt");
            }
        } else if (op_type == E_LIST) {
            return Expr(new ListFunc(parameters));//what???
        } else if (op_type == E_CONS) {
            if (parameters.size() == 2) {
                return Expr(new Cons(parameters[0], parameters[1]));
            } else {
                throw RuntimeError("Wrong number of arguments for cons");
            }
        } else if (op_type == E_CAR) {
            if (parameters.size() == 1) {
                return Expr(new Car(parameters[0]));
            } else {
                throw RuntimeError("Wrong number of arguments for car");
            }
        } else if (op_type == E_CDR) {
            if (parameters.size() == 1) {
                return Expr(new Cdr(parameters[0]));
            } else {
                throw RuntimeError("Wrong number of arguments for cdr");
            }
        } else if (op_type == E_LT) {
            //TO COMPLETE THE LOGIC
            if (parameters.size() < 2) {
                throw RuntimeError("Wrong number of arguments for <");
            }
            if (parameters.size() == 2) {
                return Expr(new Less(parameters[0], parameters[1])); 
            } else {
                return Expr(new LessVar(parameters));
            }
        } else if (op_type == E_LE) {
            //TO COMPLETE THE LOGIC
            if (parameters.size() < 2) {
                throw RuntimeError("Wrong number of arguments for <=");
            }
            if (parameters.size() == 2) {
                return Expr(new LessEq(parameters[0], parameters[1])); 
            } else {
                return Expr(new LessEqVar(parameters));
            }
        } else if (op_type == E_EQ) {
            //TO COMPLETE THE LOGIC
            if (parameters.size() < 2) {
                throw RuntimeError("Wrong number of arguments for =");
            }
            if (parameters.size() == 2) {
                return Expr(new Equal(parameters[0], parameters[1])); 
            } else {
                return Expr(new EqualVar(parameters));
            }
        } else if (op_type == E_GE) {
            //TO COMPLETE THE LOGIC
            if (parameters.size() < 2) {
                throw RuntimeError("Wrong number of arguments for >=");
            }
            if (parameters.size() == 2) {
                return Expr(new GreaterEq(parameters[0], parameters[1])); 
            } else {
                return Expr(new GreaterEqVar(parameters));
            }
        } else if (op_type == E_GT) {
            //TO COMPLETE THE LOGIC
            if (parameters.size() < 2) {
                throw RuntimeError("Wrong number of arguments for >");
            }
            if (parameters.size() == 2) {
                return Expr(new Greater(parameters[0], parameters[1])); 
            } else {
                return Expr(new GreaterVar(parameters));
            }
        } else if (op_type == E_NOT){
            if (parameters.size() == 1) {
                return Expr(new Not(parameters[0]));
            } else {
                throw RuntimeError("Wrong number of arguments for not");
            }
        } else if (op_type == E_AND) {
            return Expr(new AndVar(parameters));
        } else if (op_type == E_OR) {
            return Expr(new OrVar(parameters));
        } else if (op_type == E_EQQ) {
            if (parameters.size() == 2) {
                return Expr(new IsEq(parameters[0], parameters[1]));
            } else {
                throw RuntimeError("Wrong number of arguments for eq?");
            }
        } else if (op_type == E_BOOLQ) {
            if (parameters.size() == 1) {
                return Expr(new IsBoolean(parameters[0]));
            } else {
                throw RuntimeError("Wrong number of arguments for boolean?");
            }
        } else if (op_type == E_INTQ) {
            if (parameters.size() == 1) {
                return Expr(new IsFixnum(parameters[0]));
            } else {
                throw RuntimeError("Wrong number of arguments for number?");
            }
        } else if (op_type == E_NULLQ) {
            if (parameters.size() == 1) {
                return Expr(new IsNull(parameters[0]));
            } else {
                throw RuntimeError("Wrong number of arguments for null?");
            }
        } else if (op_type == E_PAIRQ) {
            if (parameters.size() == 1) {
                return Expr(new IsPair(parameters[0]));
            } else {
                throw RuntimeError("Wrong number of arguments for pair?");
            }
        } else if (op_type == E_PROCQ) {
            if (parameters.size() == 1) {
                return Expr(new IsProcedure(parameters[0]));
            } else {
                throw RuntimeError("Wrong number of arguments for procedure?");
            }
        } else if (op_type == E_SYMBOLQ) {
            if (parameters.size() == 1) {
                return Expr(new IsSymbol(parameters[0]));
            } else {
                throw RuntimeError("Wrong number of arguments for symbol?");
            }
        } else if (op_type == E_LISTQ) {
            if (parameters.size() == 1) {
                return Expr(new IsList(parameters[0]));
            } else {
                throw RuntimeError("Wrong number of arguments for list?");
            }
        } else if (op_type == E_STRINGQ) {
            if (parameters.size() == 1) {
                return Expr(new IsString(parameters[0]));
            } else {
                throw RuntimeError("Wrong number of arguments for string?");
            }
        } else {
            //TODO: TO COMPLETE THE LOGIC
        }
    }
    if (reserved_words.count(op) != 0) {
    	switch (reserved_words[op]) {
			//TODO: TO COMPLETE THE reserve_words PARSER LOGIC
            case E_QUOTE:{
                if (stxs.size() == 2){
                    return Expr(new Quote(stxs[1]));
                } else {
                    throw RuntimeError("Wrong number of arguments for quote");
                }
            }
            case E_BEGIN:{
                vector<Expr> es;
                for (int i = 1; i < stxs.size(); i++){
                    es.push_back(stxs[i]->parse(env));
                }
                return Expr(new Begin(es));
            }
            case E_IF:{
                if (stxs.size() == 4){
                    Expr cond = stxs[1]->parse(env);
                    Expr conseq = stxs[2]->parse(env);
                    Expr alter = stxs[3]->parse(env);
                    return Expr(new If(cond, conseq, alter));
                } else {
                    throw RuntimeError("Wrong number of arguments for if");
                }
            }
            case E_LAMBDA:{
                if (stxs.size() >= 3){
                    //stxs[1]: parameter list. 
                    vector<string> x;
                    List* param_list = dynamic_cast<List*>(stxs[1].get());
                    if (param_list == nullptr) throw RuntimeError("Wrong type of parameter list");
                    for (int i = 0; i < param_list->stxs.size(); i++) {
                        SymbolSyntax* p = dynamic_cast<SymbolSyntax*>(param_list->stxs[i].get());
                        if (p == nullptr) throw RuntimeError("Wrong type of parameter");
                        x.push_back(p->s);
                    }
                    //stxs[2...]: procedure
                    vector<Expr> es;
                    for (int i = 2; i < stxs.size(); i++){
                        es.push_back(stxs[i]->parse(env));
                    }

                    return Expr(new Lambda(x, Expr(new Begin(es))));
                }else{
                    throw RuntimeError("Wrong number of arguments for lambda");
                }
            }
            case E_DEFINE:{
                if (stxs.size() >= 3) {
                    if (auto p = dynamic_cast<SymbolSyntax*>(stxs[1].get())){
                        if (stxs.size() != 3) {
                            throw RuntimeError("Wrong number of arguments for variable define");
                        }
                        string var = p->s;
                        if (primitives.count(var) || reserved_words.count(var)) {
                            throw RuntimeError("Invalid variable name in define");
                        }
                        Expr e = stxs[2]->parse(env);
                        return Expr(new Define(var, e));
                    } else if (auto p = dynamic_cast<List*>(stxs[1].get())) {
                        //turn the simple form into name and lambda
                        if (p->stxs.empty()) {
                            throw RuntimeError("Invalid function definition in define");
                        }
                        auto p_name = dynamic_cast<SymbolSyntax*>(p->stxs[0].get());
                        if (p_name == nullptr) throw RuntimeError("Invalid function name in define");
                        string name = p_name->s;
                        vector<string> x;
                        for (int i = 1; i < p->stxs.size(); i++){
                            auto p_param = dynamic_cast<SymbolSyntax*>(p->stxs[i].get());
                            if (p_param == nullptr) throw RuntimeError("Invalid parameter name in define");
                            x.push_back(p_param->s);
                        }
                        vector<Expr> es;
                        for (int i = 2; i < stxs.size(); i++){
                            es.push_back(stxs[i]->parse(env));
                        }
                        Expr e = Expr(new Begin(es));
                        return Expr(new Define(name, Expr(new Lambda(x, e))));
                    } else {
                        throw RuntimeError("Wrong type of variable in define");
                    }
                } else {
                    throw RuntimeError("Wrong number of arguments for define");
                }
                
            }
        	default:
            	throw RuntimeError("Unknown reserved word: " + op);
    	}
    }

    //default: use Apply to be an expression
    //TODO: TO COMPLETE THE PARSER LOGIC
    Expr rator = stxs[0]->parse(env);
    vector<Expr> rands;
    for (int i = 1; i < stxs.size(); i++){
        rands.push_back(stxs[i]->parse(env));
    }
    return Expr(new Apply(rator, rands));
}
}
