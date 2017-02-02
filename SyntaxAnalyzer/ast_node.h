#pragma once
#include <memory>
#include "pooled_object.h"

namespace mq
{

class base
{
public:
    virtual ~base() = default;
};

class type : public base
{
public:
    virtual void set_innermost_type(type* type) = 0;
};

class parsing_context
{
private:
    using block_scope = std::map<std::u32string, std::unique_ptr<type>>;
    std::vector<block_scope> identifiers;
    size_t curr_scope;
public:
    parsing_context()
        : identifiers(1)
        , curr_scope(0)
    {

    }
    type* find_identifier(const std::u32string& name)
    {
        auto result = identifiers[curr_scope].find(name);
        if (result == identifiers[curr_scope].end())
        {
            return nullptr;
        }
        return result->second.get();
    }
    void scope_in()
    {
        ++curr_scope;
        if (curr_scope == identifiers.size())
        {
            identifiers.emplace_back();
        }
        else
        {
            identifiers[curr_scope].clear();
        }
    }
    void scope_out()
    {
        --curr_scope;
    }
};

class declaration : public base
{
public:
    mq::type* type;
    std::u32string name;

    explicit declaration(const std::u32string& name = U"")
        : type{}
        , name(name)
    {
    }

    void set_innermost_type(mq::type* type)
    {
        if (this->type == nullptr)
        {
            this->type = type;
        }
        else
        {
            this->type->set_innermost_type(type);
        }
    }
};

class expression : public base
{
public:
    virtual bool is_constant_expression() = 0;
};

class unary_expression : public expression
{
public:
    expression* expr;
};

class binary_expression : public expression
{
public:
    expression* lhs;
    expression* rhs;
    bool is_constant_expression() final override
    {
        return lhs->is_constant_expression() && rhs->is_constant_expression();
    }
};

class primary_expression : public expression
{

};

class identifier : public primary_expression
{
public:
    std::u32string name;

    bool is_constant_expression() final override
    {
        return false;
    }

    explicit identifier(const std::u32string& name)
        : name(name)
    {
    }
};

class constant : public primary_expression
{
public:
    bool is_constant_expression() final override
    {
        return true;
    }
};

class sint_constant : public constant
{
public:
    int value;

    explicit sint_constant(int value)
        : value(value)
    {
    }
};

class uint_constant : public constant
{
public:
    unsigned int value;

    explicit uint_constant(unsigned value)
        : value(value)
    {
    }
};

class char_constant : public constant
{
public:
    char value;

    explicit char_constant(char value)
        : value(value)
    {
    }
};

class string_literal : public primary_expression
{
public:
    std::u32string content;

    bool is_constant_expression() final override
    {
        return false;
    }

    explicit string_literal(const std::u32string& content)
        : content(content)
    {
    }
};

class postfix_expression : public expression
{
public:
    expression* expr;
};

class array_subscription : public postfix_expression
{
public:
    expression* subscriptor;

    bool is_constant_expression() final override
    {
        return expr->is_constant_expression() && subscriptor->is_constant_expression();
    }
};

class function_call : public postfix_expression
{
public:
    std::vector<expression*> arguments;

    bool is_constant_expression() override
    {
        return false;
    }
};

class address_of : public unary_expression
{
public:
    bool is_constant_expression() override
    {
        return false;
    }
};

class indirection : public unary_expression
{
public:
    bool is_constant_expression() override
    {
        return false;
    }
};

class unary_plus : public unary_expression
{
public:
    bool is_constant_expression() override
    {
        return expr->is_constant_expression();
    }
};

class unary_minus : public unary_expression
{
    bool is_constant_expression() override
    {
        return expr->is_constant_expression();
    }
};

class bitwise_negation : public unary_expression
{
    bool is_constant_expression() override
    {
        return expr->is_constant_expression();
    }
};

class logical_negation : public unary_expression
{
    bool is_constant_expression() override
    {
        return expr->is_constant_expression();
    }
};

class sizeof_type : public unary_expression
{
public:
    type* type;

    bool is_constant_expression() override
    {
        return true;
    }
};

class sizeof_expression : public unary_expression
{
public:
    bool is_constant_expression() override
    {
        return true;
    }
};

class cast_expression : public expression
{
public:
    type* target_type;

    bool is_constant_expression() override
    {
        return false;
    }
};

class multiplicative_expression : public binary_expression
{
};

class multiplication : public multiplicative_expression
{
};

class division : public multiplicative_expression
{
};

class moderation : public multiplicative_expression
{
};

class additive_expression : public binary_expression
{

};

class addition : public additive_expression
{

};

class subtraction : public additive_expression
{

};

class shift_expression : public binary_expression
{
public:
};

class left_shift : public shift_expression
{

};

class arithmetic_right_shift : public shift_expression
{

};

class logical_right_shift : public shift_expression
{

};

class relational_expression : public binary_expression
{

};

class less_than : public relational_expression
{

};

class greater_than : public relational_expression
{

};

class less_equal : public relational_expression
{

};

class greater_equal : public relational_expression
{

};

class equality_expression : public binary_expression
{

};

class equals_to : public equality_expression
{

};

class not_equals_to : public equality_expression
{

};

class bitwise_and : public binary_expression
{

};

class bitwise_exclusive_or : public binary_expression
{

};

class bitwise_inclusive_or : public binary_expression
{

};

class logical_and : public binary_expression
{

};

class logical_or : public binary_expression
{

};

class condition : public expression
{
public:
    expression* condition;
    expression* true_branch;
    expression* false_branch;

    bool is_constant_expression() override
    {
        return condition->is_constant_expression() &&
            true_branch->is_constant_expression() &&
            false_branch->is_constant_expression();
    }
};

class assignment_expression : public expression
{
public:
    expression* lhs;
    expression* rhs;

    bool is_constant_expression() override
    {
        return false;
    }
};

class basic_type : public type
{
public:
    enum kind
    {
        sint,
        uint,
        char_,
        schar,
        uchar,
        void_
    } kind;

    explicit basic_type(enum kind kind = void_)
        : kind(kind)
    {
    }

    void set_innermost_type(type* type) override
    {
        throw std::runtime_error("Error setting inner type for a basic type.");
    }
};
class derived_type : public type
{

};
class pointer_type : public derived_type
{
public:
    type* point_type;

    pointer_type()
        : point_type()
    {
    }

    void set_innermost_type(type* type) override
    {
        if (point_type == nullptr)
        {
            point_type = type;
        }
        else
        {
            point_type->set_innermost_type(type);
        }
    }
};

class array_type : public derived_type
{
public:
    type* element_type;
    std::size_t length;

    explicit array_type(size_t length)
        : element_type{}
        , length(length)
    {
    }

    void set_innermost_type(type* type) override
    {
        if (element_type == nullptr)
        {
            element_type = type;
        }
        else
        {
            element_type->set_innermost_type(type);
        }
    }
};

class function_type : public derived_type
{
public:
    type* return_type;
    std::vector<declaration*> parameters;

    function_type()
        : return_type{}
    {
    }

    void set_innermost_type(type* type) override
    {
        if (return_type == nullptr)
        {
            return_type = type;
        }
        else
        {
            return_type->set_innermost_type(type);
        }
    }
};



}
