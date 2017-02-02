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
    enum kind
    {
        arithmetic_type = 1,
        pointer_type = 2,
        array_type = 4,
        function_type = 8,
        void_type = 16,
        unsigned_type = 32,
    };
    virtual void set_innermost_type(type* type) = 0;
    virtual kind kind() = 0;
    virtual int rank()
    {
        return 0;
    }
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
    virtual type* get_type(const parsing_context& ctx) { return nullptr; /*TODO: supress compiler error*/ }
};

class implicit_conversion : public expression
{
public:
    expression* expr;
    type* target_type;
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

class explicit_cast : public expression
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

class right_shift : public shift_expression
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

/*
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
*/

class basic_type : public type
{
public:
    void set_innermost_type(type* type) final override
    {
        throw std::runtime_error("Error setting inner type for a basic type.");
    }

    enum kind kind() override
    {
        return arithmetic_type;
    }
};

class signed_int : public basic_type
{
public:
    int rank() override
    {
        return 4;
    }
};

class unsigned_int : public basic_type
{
public:
    int rank() override
    {
        return 5;
    }

    enum kind kind() override
    {
        return static_cast<enum kind>(basic_type::kind() | unsigned_type);
    }
};

class signed_char : public basic_type
{
public:
    int rank() override
    {
        return 2;
    }
};

class unsigned_char : public basic_type
{
public:
    int rank() override
    {
        return 3;
    }

    enum kind kind() override
    {
        return static_cast<enum kind>(basic_type::kind() | unsigned_type);
    }
};

class plain_char : public basic_type
{
public:
    int rank() override
    {
        return 1;
    }
};

class void_type : public basic_type
{
public:
    enum kind kind() override
    {
        return kind::void_type;
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

    enum kind kind() override
    {
        return kind::pointer_type;
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

    enum kind kind() override
    {
        return kind::array_type;
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

    enum kind kind() override
    {
        return kind::function_type;
    }
};



}
