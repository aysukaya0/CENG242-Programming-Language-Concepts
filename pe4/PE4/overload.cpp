#include "nullary.h"
#include "unary.h"
#include "binary.h"

namespace sym {
	__expr_t& operator-(const __expr_t &op) {
	    if (dynamic_cast<const __nullary_op_t*>(&op)) return *(new NegOp(op.eval()));
	    return *(new NegOp(&op));
	}
	__expr_t& exp(const __expr_t &op) {
	    if (dynamic_cast<const __nullary_op_t*>(&op)) return *(new ExpOp(op.eval()));
	    return *(new ExpOp(&op));
	}

	__expr_t& operator+(const __expr_t &lhs, const __expr_t &rhs) {
	    if (dynamic_cast<const __nullary_op_t*>(&lhs) && dynamic_cast<const __nullary_op_t*>(&rhs)) return *(new AddOp(lhs.eval(), rhs.eval()));
	    else if (dynamic_cast<const __nullary_op_t*>(&lhs)) return *(new AddOp(lhs.eval(), &rhs));
	    else if (dynamic_cast<const __nullary_op_t*>(&rhs)) return *(new AddOp(&lhs, rhs.eval()));
	    return *(new AddOp(&lhs, &rhs));
	}
	    
	__expr_t& operator+(double lhs, const __expr_t &rhs) { 
	    if(dynamic_cast<const __nullary_op_t*>(&rhs)) return *(new AddOp(new Const(lhs), rhs.eval()));
	    return *(new AddOp( new Const(lhs), &rhs));
	}
	    
	__expr_t& operator+(const __expr_t &lhs, double rhs) { 
	    if(dynamic_cast<const __nullary_op_t*>(&lhs)) return *(new AddOp(lhs.eval(),new Const(rhs)));
	    return *(new AddOp(&lhs, new Const(rhs)));
	}

	__expr_t& operator*(const __expr_t &lhs, const __expr_t &rhs) {	
	    if(dynamic_cast<const __nullary_op_t*>(&lhs) && dynamic_cast<const __nullary_op_t*>(&rhs)) return *(new MulOp(lhs.eval(), rhs.eval()));
	    else if(dynamic_cast<const __nullary_op_t*>(&lhs)) return *(new MulOp(lhs.eval(), &rhs));
	    else if(dynamic_cast<const __nullary_op_t*>(&rhs)) return *(new MulOp(&lhs, rhs.eval())); 
	    return *(new MulOp(&lhs, &rhs));
	}
	    
	__expr_t& operator*(double lhs, const __expr_t &rhs) { 
	    if(dynamic_cast<const __nullary_op_t*>(&rhs)) return *(new MulOp(new Const(lhs), rhs.eval()));
	    return *(new MulOp( new Const(lhs), &rhs));
	}
	
	__expr_t& operator*(const __expr_t &lhs, double rhs) { 
	    if(dynamic_cast<const __nullary_op_t*>(&lhs)) return *(new MulOp(lhs.eval(), new Const(rhs)));
	    return *(new MulOp(&lhs, new Const(rhs)));
	}
}