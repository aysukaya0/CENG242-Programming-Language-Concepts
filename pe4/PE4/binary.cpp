#include "binary.h"
#include "nullary.h"
#include <math.h>

namespace sym 
{
	bool AddOp::is_add() const {
	    return true;
	}

	__expr_t* AddOp::eval(const var_map_t& vars) const {
	    const Const* left = dynamic_cast<const Const*>(lhs_->eval(vars));
	    const Const* right = dynamic_cast<const Const*>(rhs_->eval(vars));
	    if (left && right) {
	        double result = left->get_value() + right->get_value();
	        delete left;
	        delete right;
	        return new Const(result);
	    }
	    else if (left && left->get_value() == 0.0){
	        delete left;
	        delete right;
	        return rhs_->eval(vars);
	    }
	    else if (right && right->get_value() == 0.0){
	        delete left;
	        delete right;
	        return lhs_->eval(vars);
	    }
	    else {
	        delete left;
	        delete right;
	        return new AddOp(lhs_->eval(vars),rhs_->eval(vars));
	    }
	}

	__expr_t* AddOp::diff(const std::string& v) const {
	    return AddOp(lhs_->diff(v),rhs_->diff(v)).eval();
	}

	std::ostream& AddOp::operator<< (std::ostream &out) const {
	    if (lhs_->is_nullary() && rhs_->is_nullary()){
	        out << *lhs_ << " + " << *rhs_;
	    }
	    else if (lhs_->is_nullary() && !rhs_->is_nullary()){
	        out << *lhs_ << " + (" << *rhs_ << ")";
	    }
	    else if (!lhs_->is_nullary() && rhs_->is_nullary()){
	        out << "(" << *lhs_ << ") + " << *rhs_;
	    }
	    else {
	        out << "(" << *lhs_<< ") + (" << *rhs_ << ")";
	    }
	    return out;
	}

	bool AddOp::operator==(const __expr_t& other_) const {
	    const AddOp* p = dynamic_cast<const AddOp*>(&other_);
	    if (p){
    	    if ((lhs_ == p->lhs_ && rhs_ == p->rhs_) || (lhs_ == p->rhs_ && rhs_ == p->lhs_)){
    	        return true;
    	    }
	    }
	    return false;
	}
}

namespace sym 
{
	bool MulOp::is_mul() const {
	    return true;
	}

	__expr_t* MulOp::eval(const var_map_t& vars) const {
	    const Const* left = dynamic_cast<const Const*>(lhs_->eval(vars));
	    const Const* right = dynamic_cast<const Const*>(rhs_->eval(vars));
	    if ((left && left->get_value() == 0) || (right && right->get_value() == 0)) {
	        delete left;
	        delete right;
	        return new Const(0.0);
	    }
	    else if (left && right){
            double result = left->get_value() * right->get_value();
            delete left;
            delete right;
            return new Const(result);
	    }
	    else if (left && left->get_value() == 1.0){
	       delete left;
	       delete right;
	       return rhs_->eval(vars);
	    }
	    else if (right && right->get_value() == 1.0){
            delete left;
            delete right;
            return lhs_->eval(vars);
	    }
	    else {
	        delete left;
	        delete right;
	        return new MulOp(lhs_->eval(vars),rhs_->eval(vars));
	    }
	}


	__expr_t* MulOp::diff(const std::string& v) const {
	    return AddOp(MulOp(lhs_->diff(v),rhs_->eval()).eval(),MulOp(lhs_->eval(),rhs_->diff(v)).eval()).eval();
	}

	std::ostream& MulOp::operator<< (std::ostream &out) const {
	    if (lhs_->is_nullary() && rhs_->is_nullary()){
	        out << *lhs_ << " * " << *rhs_;
	    }
	    else if (lhs_->is_nullary() && !rhs_->is_nullary()){
	        out << *lhs_ << " * (" << *rhs_ << ")";
	    }
	    else if (!lhs_->is_nullary() && rhs_->is_nullary()){
	        out << "(" << *lhs_ << ") * " << *rhs_;
	    }
	    else {
	        out << "(" << *lhs_<< ") * (" << *rhs_ << ")";
	    }
	    return out;
	}

	bool MulOp::operator==(const __expr_t& other_) const {
	    const MulOp* p = dynamic_cast<const MulOp*>(&other_);
	    if (p){
    	    if ((lhs_ == p->lhs_ && rhs_ == p->rhs_) || (lhs_ == p->rhs_ && rhs_ == p->lhs_)){
    	        return true;
    	    }
	    }
	    return false;
	}
}
