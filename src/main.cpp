#include <cmath>
#include <cstdint>
#include <iostream>
#include <memory>
#include <ostream>
#include <stdexcept>

class base_expr {
public:
  using base_expr_s = std::shared_ptr<base_expr>;
  virtual base_expr_s diff(base_expr_s dv) = 0;
  virtual std::ostream &display(std::ostream &os) const = 0;
  virtual bool is_constant() { return false; }
  virtual ~base_expr() = default;
};

using base_expr_s = base_expr::base_expr_s;

std::ostream &operator<<(std::ostream &os, base_expr const &be) {
  return be.display(os);
};

class constant : public base_expr {
private:
  std::double_t val;

public:
  static constexpr double E = 2.718281828459045;

  constant(std::double_t val) : val(val) {};
  base_expr_s diff(base_expr_s dv) override {
    return base_expr_s(new constant(0));
  }
  bool is_constant() override { return true; }
  std::double_t value() { return val; }

  std::ostream &display(std::ostream &os) const override { return os << val; }

  bool is_const_e() { return fabs(val - E) < 0.0000000001; }
};

class variable : public base_expr {
private:
  std::string name;

public:
  variable(std::string name) : name(name) {};
  variable(std::uint16_t coff, std::string name) : name(name) {};
  virtual base_expr_s diff(base_expr_s dv) override {
    variable const *other = static_cast<variable const *>(dv.get());
    if (name == other->name)
      return base_expr_s(new constant(1));
  }
  std::ostream &display(std::ostream &os) const override { return os << name; }
  ~variable() = default;
};

class add : public base_expr {
private:
  base_expr_s lhs;
  base_expr_s rhs;

public:
  add(base_expr_s lhs, base_expr_s rhs) : lhs(lhs), rhs(rhs) {};

  static base_expr_s create(base_expr_s lhs, base_expr_s rhs) {
    if (lhs->is_constant()) {
      constant *lhs_p = dynamic_cast<constant *>(lhs.get());
      if (lhs_p->value() == 0) {
        return rhs;
      }
    }
    if (rhs->is_constant()) {
      constant *rhs_p = dynamic_cast<constant *>(rhs.get());
      if (rhs_p->value() == 0) {
        return lhs;
      }
    }
    return base_expr_s(new add(lhs, rhs));
  }

  base_expr_s diff(base_expr_s dv) override {
    base_expr_s lhs_d(lhs->diff(dv));
    base_expr_s rhs_d(rhs->diff(dv));

    if (lhs_d->is_constant() && rhs_d->is_constant()) {
      constant *lhs_p = dynamic_cast<constant *>(lhs_d.get());
      constant *rhs_p = dynamic_cast<constant *>(rhs_d.get());
      return base_expr_s(new constant(lhs_p->value() + rhs_p->value()));
    }

    return base_expr_s(new add(lhs_d, rhs_d));
  }

  std::ostream &display(std::ostream &os) const override {
    os << '(';
    lhs->display(os);
    os << " + ";
    rhs->display(os);
    os << ')';
    return os;
  }
  ~add() = default;
};

class sub : public base_expr {
private:
  base_expr_s lhs;
  base_expr_s rhs;

public:
  sub(base_expr_s lhs, base_expr_s rhs) : lhs(lhs), rhs(rhs) {};

  static base_expr_s create(base_expr_s lhs, base_expr_s rhs) {
    if (rhs->is_constant()) {
      constant *rhs_p = dynamic_cast<constant *>(rhs.get());
      if (rhs_p->value() == 0) {
        return lhs;
      }
    }
    return base_expr_s(new sub(lhs, rhs));
  }

  virtual base_expr_s diff(base_expr_s be) override {
    base_expr_s lhs_d(lhs->diff(be));
    base_expr_s rhs_d(rhs->diff(be));

    if (lhs_d->is_constant() && rhs_d->is_constant()) {
      constant *lhs_p = dynamic_cast<constant *>(lhs_d.get());
      constant *rhs_p = dynamic_cast<constant *>(rhs_d.get());
      return base_expr_s(new constant(lhs_p->value() - rhs_p->value()));
    }

    return base_expr_s(new add(lhs_d, rhs_d));
  }
  std::ostream &display(std::ostream &os) const override {
    os << '(';
    lhs->display(os);
    os << " - ";
    rhs->display(os);
    os << ')';
    return os;
  }
  ~sub() = default;
};

class mul : public base_expr {
  base_expr_s lhs;
  base_expr_s rhs;

public:
  mul(base_expr_s lhs, base_expr_s rhs) : lhs(lhs), rhs(rhs) {};

  static base_expr_s create(base_expr_s lhs, base_expr_s rhs) {
    if (lhs->is_constant()) {
      constant *lhs_p = dynamic_cast<constant *>(lhs.get());
      if (lhs_p->value() == 0) {
        return base_expr_s(new constant(0));
      }
      if (lhs_p->value() == 1) {
        return rhs;
      }
    }
    if (rhs->is_constant()) {
      constant *rhs_p = dynamic_cast<constant *>(rhs.get());
      if (rhs_p->value() == 0) {
        return base_expr_s(new constant(0));
      }
      if (rhs_p->value() == 1) {
        return lhs;
      }
    }
    return base_expr_s(new mul(lhs, rhs));
  }

  virtual base_expr_s diff(base_expr_s be) override {
    base_expr_s lhs_d(lhs->diff(be));
    base_expr_s rhs_d(rhs->diff(be));

    return add::create(base_expr_s(mul::create(lhs, rhs_d)),
                       base_expr_s(mul::create(lhs_d, rhs)));
  }

  std::ostream &display(std::ostream &os) const override {
    os << '(';
    lhs->display(os);
    os << " * ";
    rhs->display(os);
    os << ')';
    return os;
  }
  ~mul() = default;
};

class pow : public base_expr {
  base_expr_s lhs;
  base_expr_s rhs;

public:
  pow(base_expr_s lhs, base_expr_s rhs) : lhs(lhs), rhs(rhs) {};

  static base_expr_s create(base_expr_s lhs, base_expr_s rhs) {
    if (!lhs->is_constant() && rhs->is_constant()) {
      constant *rhs_p = dynamic_cast<constant *>(rhs.get());
      if (rhs_p->value() == 0) {
        return base_expr_s(new constant(1));
      }
      if (rhs_p->value() == 1) {
        return lhs;
      }
    }
    return base_expr_s(new pow(lhs, rhs));
  }

  virtual base_expr_s diff(base_expr_s be) override;

  std::ostream &display(std::ostream &os) const override {
    os << '(';
    lhs->display(os);
    os << " ^ ";
    rhs->display(os);
    os << ')';
    return os;
  }
  ~pow() = default;
};

class div : public base_expr {
  base_expr_s lhs;
  base_expr_s rhs;

public:
  div(base_expr_s lhs, base_expr_s rhs) : lhs(lhs), rhs(rhs) {};

  static base_expr_s create(base_expr_s lhs, base_expr_s rhs) {
    if (rhs->is_constant()) {
      constant *rhs_p = dynamic_cast<constant *>(rhs.get());
      if (rhs_p->value() == 0) {
        throw std::runtime_error("math error: attempted to divide by zero");
      }
      if (rhs_p->value() == 1) {
        return lhs;
      }
    }
    if (lhs->is_constant()) {
      constant *lhs_p = dynamic_cast<constant *>(lhs.get());
      if (lhs_p->value() == 0) {
        return base_expr_s(new constant(0));
      }
    }
    return base_expr_s(new div(lhs, rhs));
  }

  virtual base_expr_s diff(base_expr_s be) override {
    base_expr_s lhs_d(lhs->diff(be));
    base_expr_s rhs_d(rhs->diff(be));

    return div::create(
        sub::create(mul::create(lhs, rhs_d), mul::create(rhs_d, lhs)),
        pow::create(rhs, base_expr_s(new constant(2))));
  }

  std::ostream &display(std::ostream &os) const override {
    os << '(';
    lhs->display(os);
    os << " / ";
    rhs->display(os);
    os << ')';
    return os;
  }
  ~div() = default;
};

class ln : public base_expr {
  base_expr_s value;

public:
  ln(base_expr_s value) : value(value) {};
  static base_expr_s create(base_expr_s value) {
    if (value->is_constant()) {
      constant *value_p = dynamic_cast<constant *>(value.get());
      if (value_p->value() == 0) {
        throw std::runtime_error("math error: argument of ln is zero");
      }
      if (value_p->is_const_e()) {
        return base_expr_s(new constant(1));
      }
    }
    return base_expr_s(new ln(value));
  }
  virtual base_expr_s diff(base_expr_s be) override {
    base_expr_s value_d(value->diff(be));

    return mul::create(div::create(base_expr_s(new constant(1)), value),
                       value_d);
  }

  std::ostream &display(std::ostream &os) const override {
    os << " ln(";
    value->display(os);
    os << ')';
    return os;
  }
  ~ln() = default;
};

base_expr_s pow::diff(base_expr_s be) {
  if (!lhs->is_constant() && rhs->is_constant()) {
    base_expr_s lhs_d(lhs->diff(be));
    constant *rhs_p = dynamic_cast<constant *>(rhs.get());
    base_expr_s new_power(new constant(rhs_p->value() - 1));
    return mul::create(mul::create(rhs, pow::create(lhs, new_power)), lhs_d);
  }
  if (lhs->is_constant() && !rhs->is_constant()) {
    base_expr_s rhs_d(rhs->diff(be));
    return mul::create(mul::create(pow::create(lhs, rhs), ln::create(lhs)),
                       rhs_d);
  }
}

int main(int argc, char *argv[]) {
  base_expr_s x(new variable("x"));
  base_expr_s c1(new constant(69));
  base_expr_s c2(new constant(420));
  base_expr_s c3(new constant(5));
  base_expr_s c4(new constant(constant::E));

  base_expr_s pow1(add::create(mul::create(c3, pow::create(x, c1)),
                               mul::create(c3, pow::create(x, c2))));
  base_expr_s pow2(pow::create(c3, mul::create(c1, x)));
  base_expr_s pow3(pow::create(c4, mul::create(c1, x)));

  base_expr_s pow1d(pow1->diff(x));
  base_expr_s pow2d(pow2->diff(x));
  base_expr_s pow3d(pow3->diff(x));

  std::cout << *pow1 << "\t:\t" << *pow1d << std::endl;
  std::cout << *pow2 << "\t:\t" << *pow2d << std::endl;
  std::cout << *pow3 << "\t:\t" << *pow3d << std::endl;
  return 0;
}
