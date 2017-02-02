#pragma once
#include "../LexicalAnalyzer/lexical_analyzer.h"
#include "ast_node.h"
#include <cstdint>
#include <algorithm>
#include "../../../../../../../Program Files (x86)/Microsoft Visual Studio 14.0/VC/include/iso646.h"

namespace mq
{

struct syntax_error : std::runtime_error
{
    syntax_error(const std::string& message, int line, int column)
        : runtime_error(message + " at " + std::to_string(line) + '(' + std::to_string(column) + ')')
        , message(message)
        , line(line)
        , column(column)
    {
    }

    std::string message;
    int line;
    int column;
};

class syntax_analyzer
{
public:
    lexical_analyzer& lex;

    parsing_context ctx;

    explicit syntax_analyzer(lexical_analyzer& lex)
        : lex(lex)
    {
    }

    pooled_object<expression, base> parse_expression()
    {
        //TODO: finish it
        return nullptr;
    }

    pooled_object<constant, base> parse_constant()
    {
        auto token = lex.peek();
        switch (token.type)
        {
        case token_type::char_literal:
        {
            lex.next();
            return make_pooled_object<char_constant>(static_cast<char>(token.content[0] & 0x7F));
        }
        case token_type::number_literal:
        {
            lex.next();
            auto num = get_integer(token);
            if (num > std::numeric_limits<int>::max())
            {
                return make_pooled_object<uint_constant>(num);
            }
            return make_pooled_object<sint_constant>(num);
        }
        default:
            error("Unexpected token in parsing constant", token);
        }
    }

    pooled_object<expression, base> parse_relational_expression()
    {
        auto result = parse_shift_expression();
        for(;;)
        {
            pooled_object<relational_expression, base> e;
            switch(lex.peek().type)
            {
            case token_type::lt:
                e = make_pooled_object<less_than>();
                break;
            case token_type::gt:
                e = make_pooled_object<greater_than>();
                break;
            case token_type::le:
                e = make_pooled_object<less_equal>();
                break;
            case token_type::ge:
                e = make_pooled_object<greater_equal>();
                break;
            default:
                return result;
            }
            lex.next();
            e->lhs = e.merge(result);
            e->rhs = e.merge(parse_shift_expression());
            result = std::move(e);
        }
    }

    pooled_object<expression, base> parse_shift_expression()
    {
        auto result = parse_additive_expression();
        for (;;)
        {
            pooled_object<shift_expression, base> e;
            switch (lex.peek().type)
            {
            case token_type::shl:
            {
                e = make_pooled_object<left_shift>();
            }
            break;
            case token_type::shr:
            {
                //TODO: get context and finish it
            }
            break;
            default:
                return result;
            }
            lex.next();
            e->lhs = e.merge(result);
            e->rhs = e.merge(parse_additive_expression());
            result = std::move(e);
        }
    }

    pooled_object<expression, base> parse_additive_expression()
    {
        auto result = parse_multiplicative_expression();
        for (;;)
        {
            pooled_object<additive_expression, base> e;
            switch (lex.peek().type)
            {
            case token_type::plus:
            {
                e = make_pooled_object<addition>();
            }
            break;
            case token_type::sub:
            {
                e = make_pooled_object<subtraction>();
            }
            break;
            default:
                return result;
            }
            lex.next();
            e->lhs = e.merge(result);
            e->rhs = e.merge(parse_multiplicative_expression());
            result = std::move(e);
        }
    }

    pooled_object<expression, base> parse_multiplicative_expression()
    {
        auto result = parse_cast_expression();
        for (;;)
        {
            pooled_object<multiplicative_expression, base> e;
            switch (lex.peek().type)
            {
            case token_type::mod:
            {
                e = make_pooled_object<moderation>();
            }
            break;
            case token_type::div:
            {
                e = make_pooled_object<division>();
            }
            break;
            case token_type::mul:
            {
                e = make_pooled_object<multiplication>();
            }
            break;
            default:
                return result;
            }
            lex.next();
            e->lhs = e.merge(result);
            e->rhs = e.merge(parse_cast_expression());
            result = std::move(e);
        }
    }

    pooled_object<expression, base> parse_cast_expression()
    {
        if (lex.peek().type == token_type::open_parentheses)
        {
            pooled_object<expression, base> r;
            auto ty = parse_type();
            expect(token_type::close_parentheses);
            pooled_object<cast_expression, base> e = make_pooled_object<cast_expression>();
            e->target_type = e.merge(ty);
            r = std::move(e);
            return r;
        }
        return parse_unary_expression();
    }

    pooled_object<expression, base> parse_unary_expression()
    {
        auto token = lex.peek();
        pooled_object<unary_expression, base> result;
        switch (token.type)
        {
        case token_type::size_of:
        {
            lex.next();
            auto next = lex.peek();
            if (next.type == token_type::open_parentheses)
            {
                auto type = parse_type();
                pooled_object<sizeof_type, base> t = make_pooled_object<sizeof_type>();
                t->type = t.merge(type);
                result = std::move(t);
                return std::move(result);
            }
            result = make_pooled_object<sizeof_expression>();
            result->expr = result.merge(parse_unary_expression());
            return std::move(result);
        }
        break;
        case token_type::bit_and:
        {
            lex.next();
            result = make_pooled_object<address_of>();
        }
        break;
        case token_type::asterisk:
        {
            lex.next();
            result = make_pooled_object<indirection>();
        }
        break;
        case token_type::plus:
        {
            lex.next();
            result = make_pooled_object<unary_plus>();
        }
        break;
        case token_type::sub:
        {
            lex.next();
            result = make_pooled_object<unary_minus>();
        }
        break;
        case token_type::not_:
        {
            lex.next();
            result = make_pooled_object<logical_negation>();
        }
        break;
        case token_type::bit_not:
        {
            lex.next();
            result = make_pooled_object<bitwise_negation>();
        }
        break;
        default:
        {
            return parse_postfix_expression();
        }
        }
        result->expr = result.merge(parse_cast_expression());
        return std::move(result);
    }

    pooled_object<expression, base> parse_postfix_expression()
    {
        auto result = parse_primary_expression();;
        for (;;)
        {
            auto token = lex.peek();
            switch (token.type)
            {
            case token_type::open_parentheses:
            {
                lex.next();
                pooled_object<function_call, base> res = make_pooled_object<function_call>();
                res->expr = res.merge(result);
                while (lex.peek().type != token_type::close_parentheses)
                {
                    auto argument = parse_expression();
                    res->arguments.push_back(res.merge(argument));
                    expect(token_type::comma);
                }
                lex.next();
                result = std::move(res);
            }
            case token_type::open_bracket:
            {
                lex.next();
                auto subscriptor = parse_expression();
                expect(token_type::close_bracket);
                pooled_object<array_subscription, base> res = make_pooled_object<array_subscription>();
                res->expr = res.merge(result);
                res->subscriptor = res.merge(subscriptor);
                result = std::move(res);
            }
            default:
                return result;
            }
        }
    }

    pooled_object<expression, base> parse_primary_expression()
    {
        auto token = lex.peek();
        switch (token.type)
        {
        case token_type::identifier:
            return make_pooled_object<identifier>(token.content);
        case token_type::number_literal:
            return parse_constant();
        case token_type::string_literal:
            return make_pooled_object<string_literal>(token.content);
        case token_type::open_parentheses:
        {
            lex.next();
            auto expr = parse_expression();
            expect(token_type::close_parentheses);
            return expr;
        }
        default:
            error("Unexpected token when parsing primary expression", token);
        }
    }

    pooled_object<basic_type, base> parse_basic_type()
    {
        auto token = lex.peek();
        switch (token.type)
        {
        case token_type::type_signed:
        {
            lex.next();
            auto ty = lex.peek();
            switch (ty.type)
            {
            case token_type::type_int:
                lex.next();
            default: //fall through
                return make_pooled_object<basic_type>(basic_type::sint);
            case token_type::type_char:
                lex.next();
                return make_pooled_object<basic_type>(basic_type::schar);
            }
        }
        break;
        case token_type::type_unsigned:
        {
            lex.next();
            auto ty = lex.peek();
            switch (ty.type)
            {
            case token_type::type_int:
                lex.next();
            default: //fall through
                return make_pooled_object<basic_type>(basic_type::uint);
            case token_type::type_char:
                lex.next();
                return make_pooled_object<basic_type>(basic_type::uchar);
            }
        }
        break;
        case token_type::type_int:
        {
            lex.next();
            return make_pooled_object<basic_type>(basic_type::sint);
        }
        case token_type::type_char:
        {
            lex.next();
            return make_pooled_object<basic_type>(basic_type::char_);
        }
        case token_type::type_void:
        {
            lex.next();
            return make_pooled_object<basic_type>(basic_type::void_);
        }
        default:
        {
            error("Here a type expected.", token);
        }
        } //switch
    }

    pooled_object<type, base> parse_type()
    {
        auto decl = parse_declaration_or_type<false>();
        pooled_object<type, base> result{decl->type};
        decl.pool().front().release();
        std::for_each(decl.pool().begin() + 1, decl.pool().end(), [&result](auto& ptr)
        {
            result.adopt(ptr);
        });
        return result;
    }

    template<bool IsIdentifierAllowed>
    pooled_object<declaration, base> parse_declaration_or_type()
    {
        std::u32string name;
        pooled_object<declaration, base> decl;
        switch (lex.peek().type)
        {
        case token_type::type_int:
        case token_type::type_char:
        case token_type::type_void:
        case token_type::type_unsigned:
        case token_type::type_signed:
        {
            auto type = parse_basic_type();
            decl = parse_declaration_or_type<IsIdentifierAllowed>();
            decl->set_innermost_type(decl.merge(type));
        }
        break;
        case token_type::asterisk:
        {
            lex.next();
            decl = parse_declaration_or_type<IsIdentifierAllowed>();
            decl->set_innermost_type(decl.emplace<pointer_type>());
        }
        break;
        case token_type::open_parentheses:
        {
            lex.next();
            decl = parse_declaration_or_type<IsIdentifierAllowed>();
            expect(token_type::close_parentheses);
        }
        break;
        case token_type::identifier:
        {
            if (!IsIdentifierAllowed)
            {
                error("Unexpected identifier in parsing a type.", lex.peek());
            }
            decl = make_pooled_object<declaration>(lex.next().content);
        }
        break;
        default://may be just a type, allow it
        {
            if (IsIdentifierAllowed)
            {
                error("Expected an identifer in parsing a declaration.", lex.peek());
            }
            decl = make_pooled_object<declaration>();
        }
        break;
        } //switch
        switch (lex.peek().type)
        {
        case token_type::open_bracket: //[, array type
        {
            lex.next();
            auto size = get_positive_integer(lex.next());
            decl->type = decl.emplace<array_type>(size);
            expect(token_type::close_bracket);
        }
        break;
        case token_type::open_parentheses:
        {
            lex.next();
            auto functionType = decl.emplace<function_type>();
            decl->type = functionType;

            while (lex.peek().type != token_type::close_parentheses)
            {
                auto paramDecl = parse_declaration_or_type<true>();
                if (paramDecl->type == nullptr)
                {
                    error("Syntax error near here.", lex.peek());
                }
                functionType->parameters.push_back(decl.merge(paramDecl));
                switch (lex.peek().type)
                {
                case token_type::comma:
                {
                    lex.next();
                }
                break;
                case token_type::close_parentheses:
                { //let it go
                }
                break;
                default:
                {
                    error("Expected a ',' or ')'", lex.peek());
                }
                }
            }
        }
        break;
        default:;//let it be
        }
        return decl;
    }

    void expect(token_type type, const std::string& msg = "")
    {
        if (lex.peek().type != type)
        {
            error("Expected a " + (msg == "" ? token_type_strs[(int)type] : msg) + ".", lex.peek());
        }
        lex.next();
    }

    static std::int64_t get_integer(const code_token& token)
    {
        int prefixIndex = 0;
        if (token.content[0] == '-' || token.content[0] == '+')
        {
            prefixIndex++;
        }
        int base = 10;
        if (token.content.size() > prefixIndex && token.content[prefixIndex] == '0')
        {
            base = 8;
            prefixIndex++;
            if (token.content.size() > prefixIndex &&
                (token.content[prefixIndex] == 'x' || token.content[prefixIndex] == 'X'))
            {
                base = 10;
            }
        }
        std::string ansiString{token.content.begin(), token.content.end()};
        size_t index = 0;
        int64_t num = std::stoll(ansiString, &index, base);
        if (index != ansiString.size())
        {
            error("Invalid number constant.", token);
        }
        if (num > std::numeric_limits<unsigned int>::max())
        {
            error("Number constant overflow unsigned int.", token);
        }
        if (num < std::numeric_limits<int>::min())
        {
            error("Number constant underflow int", token);
        }
        return num;
    }

    static unsigned int get_positive_integer(const code_token& token)
    {
        auto num = get_integer(token);
        if (num < 0)
        {
            error("Expected a positive integer", token);
        }
        return num;
    }

    [[noreturn]]
    static void error(const std::string& msg, const code_token& tok)
    {
        throw syntax_error{msg, tok.line, tok.column};
    }
};

}