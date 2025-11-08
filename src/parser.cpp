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
    throw RuntimeError("Unimplemented parse method");//Then why declare a parse in Syntax?
}

Expr Number::parse(Assoc &env) {
    return Expr(new Fixnum(n));
}

Expr RationalSyntax::parse(Assoc &env) {//may need: simplify; check 0. 
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
    SymbolSyntax *id = dynamic_cast<SymbolSyntax*>(stxs[0].get());//check whether is a pointer to SymbolSyntax
    if (id == nullptr) {//dynamic cast failed
        //TODO: TO COMPLETE THE LOGIC
    }else{
    string op = id->s;
    if (find(op, env).get() != nullptr) {//check whether it's a variable?//return?
        //TODO: TO COMPLETE THE PARAMETER PARSER LOGIC
    }
    if (primitives.count(op) != 0) {//whether in primitive map
        vector<Expr> parameters;
        //TODO: TO COMPLETE THE PARAMETER PARSER LOGIC
        for (int i = 1; i < stxs.size(); i++){//what does modulo need?
            if(auto p_Number = dynamic_cast<Number*>(stxs[i].get())){
                parameters.push_back(Expr(new Fixnum(p_Number->n)));
            }else{
                throw RuntimeError("Parameter is not a number");
            }
        }
        ExprType op_type = primitives[op];
        if (op_type == E_PLUS) {
            if (parameters.size() == 2) {
                return Expr(new Plus(parameters[0], parameters[1])); 
            } else {
                throw RuntimeError("Wrong number of arguments for +");//???
            }
        } else if (op_type == E_MINUS) {
            //TODO: TO COMPLETE THE LOGIC
            if (parameters.size() == 2) {
                return Expr(new Minus(parameters[0], parameters[1])); 
            } else {
                throw RuntimeError("Wrong number of arguments for -");//???
            }
        } else if (op_type == E_MUL) {
            //TODO: TO COMPLETE THE LOGIC
            if (parameters.size() == 2) {
                return Expr(new Mult(parameters[0], parameters[1])); 
            } else {
                throw RuntimeError("Wrong number of arguments for *");//???
            }
        }  else if (op_type == E_DIV) {
            //TODO: TO COMPLETE THE LOGIC
            if (parameters.size() == 2) {
                auto p_Fixnum = dynamic_cast<Fixnum*>(parameters[1].get());
                if(p_Fixnum->n == 0){
                    throw RuntimeError("Division by zero");
                }
                }
                return Expr(new Div(parameters[0], parameters[1])); 
            } else {
                throw RuntimeError("Wrong number of arguments for /");//???
            }
        } else if (op_type == E_MODULO) {
            if (parameters.size() != 2) {
                throw RuntimeError("Wrong number of arguments for modulo");
            }
            return Expr(new Modulo(parameters[0], parameters[1]));//???what is expt???
        } else if (op_type == E_LIST) {
            return Expr(new ListFunc(parameters));
        } else if (op_type == E_LT) {
            //TODO: TO COMPLETE THE LOGIC
        } else if (op_type == E_LE) {
            //TODO: TO COMPLETE THE LOGIC
        } else if (op_type == E_EQ) {
            //TODO: TO COMPLETE THE LOGIC
        } else if (op_type == E_GE) {
            //TODO: TO COMPLETE THE LOGIC
        } else if (op_type == E_GT) {
            //TODO: TO COMPLETE THE LOGIC
        } else if (op_type == E_AND) {
            return Expr(new AndVar(parameters));
        } else if (op_type == E_OR) {
            return Expr(new OrVar(parameters));
        } else {
            //TODO: TO COMPLETE THE LOGIC
        }
    }

    if (reserved_words.count(op) != 0) {
    	switch (reserved_words[op]) {
			//TODO: TO COMPLETE THE reserve_words PARSER LOGIC
        	default:
            	throw RuntimeError("Unknown reserved word: " + op);
    	}
    }

    //default: use Apply to be an expression
    //TODO: TO COMPLETE THE PARSER LOGIC
}//else
}//k=list
