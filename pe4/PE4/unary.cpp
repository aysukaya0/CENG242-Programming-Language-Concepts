#include "nullary.h"
#include "unary.h"
#include "binary.h"
#include <math.h>

namespace sym 
{
	bool NegOp::is_neg() const {
	    return true;
	}

	__expr_t* NegOp::eval(const var_map_t& vars) const {
	    Const* p = dynamic_cast<Const*>(operand->eval(vars));
	    if (p){
	        double val = p->get_value();
	        delete p;
	        return new Const(-val);
	    }
	    return new NegOp(operand->eval(vars));
	}

	__expr_t* NegOp::diff(const std::string& v) const {
	    return NegOp(operand->diff(v)).eval();
	}

	std::ostream& NegOp::operator<< (std::ostream &out) const {
	    if (operand->is_nullary()) {
	        out << "-" << *operand;
	    }
	    else { out << "-(" << *operand << ")"; }
	    return out;
	}

	bool NegOp::operator==(const __expr_t& other_) const {
	    const NegOp* otherVar = dynamic_cast<const NegOp*>(&other_);
	    if (otherVar) { 
	        if (otherVar == operand){
	            return true;
	        }
	    }
	    return false;
	}
}

namespace sym 
{
	bool ExpOp::is_exp() const {
	    return true;
	}
	
	__expr_t* ExpOp::eval(const var_map_t& vars) const {
	    Const* p = dynamic_cast<Const*>(operand->eval(vars));
	    if (p){
	        double val = p->get_value();
	        double result = std::exp(val);
	        delete p;
	        return new Const(result);
	    }
	    return new ExpOp(operand->eval(vars));
	}
	
	__expr_t* ExpOp::diff(const std::string& v) const {
	    return MulOp(operand->diff(v),new ExpOp(operand->eval())).eval();
	}

	std::ostream& ExpOp::operator<< (std::ostream &out) const {
	    if (operand->is_nullary()) {
	        out << "e^" << *operand;
	    }
	    else { out << "e^(" << *operand << ")"; }
	    return out;
	}

	bool ExpOp::operator==(const __expr_t& other_) const {
	    const ExpOp* otherVar = dynamic_cast<const ExpOp*>(&other_);
	    if (otherVar) { 
	        if (otherVar == operand){
	            return true;
	        }
	    }
	    return false;
	}
}











