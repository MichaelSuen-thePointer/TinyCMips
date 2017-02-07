#pragma once
#include "../LexicalAnalyzer/lexical_analyzer.h"
#include "ast_node.h"
#include <cstdint>
#include <algorithm>

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

    pooled_object<statement, base> parse_statement()
    {
        switch (lex.peek().type)
        {
        case token_type::open_brace: //compound statement
            return parse_compound_statement();
        case token_type::keyword_if:
            return parse_selection_statement();
        case token_type::keyword_while:
            return parse_iteration_statement();
        case token_type::keyword_break:
        case token_type::keyword_continue:
            return parse_jump_statement();
        default:
            return parse_expression_statement();
        }
    }

    pooled_object<declaration, base> parse_declaration()
    {
        auto name = parse_declaration_or_type<true>();
        switch (lex.peek().type)
        {
        case token_type::semicolon:
            lex.next();
            break;
        case token_type::assign:
        {
            lex.next();
            auto initializer = parse_initializer();
            name->initializer = name.merge(initializer);
            expect(token_type::semicolon);
        }
        break;
        case token_type::open_brace:
        { //function definition
            if (name->type->kind() & type::function_type)
            {
                auto body = parse_compound_statement();
                pooled_object<function_body, base> initializer = make_pooled_object<function_body>();
                initializer->body = initializer.merge(body);
                name->initializer = name.merge(initializer);
            }
        }
        break;
        default:
            error("Unexpected token in parsing declaration.", lex.peek());
        }
        return name;
    }

    pooled_object<initializer, base> parse_initializer()
    {
        if (lex.peek().type == token_type::open_brace)
        {
            lex.next();
            pooled_object<array_initializer, base> list = make_pooled_object<array_initializer>();
            while (lex.peek().type != token_type::close_brace)
            {
                list->initializer_list.push_back(list.merge(parse_initializer()));
                if (lex.peek().type == token_type::colon)
                {
                    lex.next();
                }
            }
            expect(token_type::close_brace);
            return std::move(list);
        }
        else
        {
            pooled_object<expression_initializer, base> init = make_pooled_object<expression_initializer>();
            init->expr = init.merge(parse_expression());
            return std::move(init);
        }
    }

    pooled_object<compound_statement, base> parse_compound_statement()
    {
        pooled_object<compound_statement, base> res = make_pooled_object<compound_statement>();
        expect(token_type::open_brace);
        ctx.scope_in();
        while (lex.peek().type != token_type::close_brace)
        {
            switch (lex.peek().type)
            {
            case token_type::type_char:
            case token_type::type_int:
            case token_type::type_signed:
            case token_type::type_unsigned:
            case token_type::type_void:
                res->declaration_list.push_back(res.merge(parse_declaration()));
                if (ctx.add_identifier(res->declaration_list.back()) == false)
                {
                    error("Duplicated identifier name in current scope", lex.peek());
                }
                continue;
            default:;//fall through
            }
            break;
        }
        while (lex.peek().type != token_type::close_brace)
        {
            res->statement_list.push_back(res.merge(parse_statement()));
        }
        expect(token_type::close_brace);
        ctx.scope_out();
        return res;
    }

    pooled_object<jump_statement, base> parse_jump_statement()
    {
        switch (lex.peek().type)
        {
        case token_type::keyword_break:
            lex.next();
            return make_pooled_object<break_statment>();
        case token_type::keyword_continue:
            lex.next();
            return make_pooled_object<continue_statement>();
        case token_type::keyword_return:
        {
            lex.next();
            pooled_object<return_statement, base> retStmt = make_pooled_object<return_statement>();
            if (lex.peek().type != token_type::semicolon)
            {
                retStmt->expr = retStmt.merge(parse_expression());
            }
            return std::move(retStmt);
        }
        default:
            error("Unexpected token in parsing jump statement", lex.peek());
        }
    }

    pooled_object<iteration_statement, base> parse_iteration_statement()
    {
        expect(token_type::keyword_while);
        expect(token_type::open_parentheses);
        auto cond = parse_expression();
        expect(token_type::close_parentheses);
        auto body = parse_statement();
        pooled_object<iteration_statement, base> res = make_pooled_object<iteration_statement>();
        res->condition = res.merge(cond);
        res->body = res.merge(body);
        return res;
    }

    pooled_object<selection_statement, base> parse_selection_statement()
    {
        pooled_object<selection_statement, base> stmt = make_pooled_object<selection_statement>();
        expect(token_type::keyword_if);
        expect(token_type::open_parentheses);
        auto cond = parse_expression();
        expect(token_type::close_parentheses);
        auto ifBody = parse_statement();
        stmt->condition = stmt.merge(cond);
        stmt->ture_branch = stmt.merge(ifBody);
        if (lex.peek().type == token_type::keyword_else)
        {
            lex.next();
            auto elseBody = parse_statement();
            stmt->false_branch = stmt.merge(elseBody);
        }
        return stmt;
    }

    pooled_object<expression_statement, base> parse_expression_statement()
    {
        auto expr = parse_expression();
        expect(token_type::semicolon);
        pooled_object<expression_statement, base> expstm = make_pooled_object<expression_statement>();
        expstm->expr = expstm.merge(expr);
        return expstm;
    }

    template<class T1, class T2>
    void do_implicit_conversion(pooled_object<T1, T2>& expr)
    {
        auto lType = expr->lhs->get_type(ctx);
        auto rType = expr->lhs->get_type(ctx);

        auto lty = lType->kind();
        auto rty = rType->kind();

        if (lty & type::arithmetic_type && rty & type::arithmetic_type)
        {
            if (lType->rank() > rType->rank())
            {
                auto implicitConv = make_pooled_object<implicit_conversion>();
                implicitConv->target_type = rType;
                implicitConv->expr = expr->rhs;
                expr->rhs = expr.merge(implicitConv);
            }
            else if (lType->rank() < rType->rank())
            {
                auto implicitConv = make_pooled_object<implicit_conversion>();
                implicitConv->target_type = lType;
                implicitConv->expr = expr->lhs;
                expr->lhs = expr.merge(implicitConv);
            }
        }
        if ((lty & type::array_type && rty & type::arithmetic_type) ||
            (lty & type::arithmetic_type && rty & type::array_type))
        {
            pooled_object<implicit_conversion, base> implicitConv = make_pooled_object<implicit_conversion>();
            if (lty == type::array_type)
            {
                auto ptrToElementType = std::make_unique<pointer_type>();
                ptrToElementType->point_type = dynamic_cast<array_type*>(lType)->element_type;
                implicitConv->target_type = implicitConv.adopt(ptrToElementType);
                expr->lhs = expr.merge(implicitConv);
            }
            else
            {
                auto ptrToElementType = std::make_unique<pointer_type>();
                ptrToElementType->point_type = dynamic_cast<array_type*>(rType)->element_type;
                implicitConv->target_type = implicitConv.adopt(ptrToElementType);
                expr->rhs = expr.merge(implicitConv);
            }
        }
    }
    //unray-expression:
    //      identifier
    //      constant
    //      string-literal
    //      ( expression )
    //      & * + - ~ !
    //      sizeof
    bool is_unary_expression_start()
    {
        switch (lex.next().type)
        {
        case token_type::identifier:
        case token_type::number_literal:
        case token_type::string_literal:
        case token_type::open_parentheses:
        case token_type::bit_and:
        case token_type::asterisk:
        case token_type::plus:
        case token_type::sub:
        case token_type::bit_not:
        case token_type::not_:
            return true;
        default:
            return false;
        }
    }

    pooled_object<expression, base> parse_assignment_expression()
    {
        auto lhs = parse_logical_or_expression();
        if (lhs->is_unary_expression() &&
            lex.peek().type == token_type::assign)
        {
            auto rhs = parse_logical_or_expression();
            pooled_object<assignment_expression, base> expr = make_pooled_object<assignment_expression>();
            expr->lhs = expr.merge(lhs);
            expr->rhs = expr.merge(rhs);
            return std::move(expr);
        }
        return lhs;
    }

    pooled_object<expression, base> parse_expression()
    {
        return parse_logical_or_expression();
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

    pooled_object<expression, base> parse_logical_or_expression()
    {
        auto result = parse_logical_and_expression();
        while (lex.peek().type == token_type::logical_or)
        {
            lex.next();
            pooled_object<logical_or, base> e = make_pooled_object<logical_or>();
            e->lhs = e.merge(result);
            e->rhs = e.merge(parse_logical_and_expression());
            result = std::move(e);
        }
        return result;
    }

    pooled_object<expression, base> parse_logical_and_expression()
    {
        auto result = parse_bitwise_inclusive_or_expression();
        while (lex.peek().type == token_type::logical_and)
        {
            lex.next();
            pooled_object<logical_and, base> e = make_pooled_object<logical_and>();
            e->lhs = e.merge(result);
            e->rhs = e.merge(parse_bitwise_inclusive_or_expression());
            result = std::move(e);
        }
        return result;
    }

    pooled_object<expression, base> parse_bitwise_inclusive_or_expression()
    {
        auto result = parse_bitwise_exclusive_or_expression();
        while (lex.peek().type == token_type::bit_or)
        {
            lex.next();
            pooled_object<bitwise_inclusive_or, base> e = make_pooled_object<bitwise_inclusive_or>();
            e->lhs = e.merge(result);
            e->rhs = e.merge(parse_bitwise_exclusive_or_expression());
            do_implicit_conversion(e);
            result = std::move(e);
        }
        return result;
    }

    pooled_object<expression, base> parse_bitwise_exclusive_or_expression()
    {
        auto result = parse_bitwise_and_expression();
        while (lex.peek().type == token_type::xor_)
        {
            lex.next();
            pooled_object<bitwise_exclusive_or, base> e = make_pooled_object<bitwise_exclusive_or>();
            e->lhs = e.merge(result);
            e->rhs = e.merge(parse_bitwise_and_expression());
            do_implicit_conversion(e);
            result = std::move(e);
        }
        return result;
    }

    pooled_object<expression, base> parse_bitwise_and_expression()
    {
        auto result = parse_equlity_expression();
        while (lex.peek().type == token_type::bit_and)
        {
            lex.next();
            pooled_object<bitwise_and, base> e = make_pooled_object<bitwise_and>();
            e->lhs = e.merge(result);
            e->rhs = e.merge(parse_equlity_expression());
            do_implicit_conversion(e);
            result = std::move(e);
        }
        return result;
    }

    pooled_object<expression, base> parse_equlity_expression()
    {
        auto result = parse_relational_expression();
        for (;;)
        {
            pooled_object<equality_expression, base> e;
            switch (lex.peek().type)
            {
            case token_type::eq:
                e = make_pooled_object<equals_to>();
                break;
            case token_type::ne:
                e = make_pooled_object<not_equals_to>();
                break;
            default:
                return result;
            }
            lex.next();
            e->lhs = e.merge(result);
            e->rhs = e.merge(parse_relational_expression());
            do_implicit_conversion(e);
            result = std::move(e);
        }
    }

    pooled_object<expression, base> parse_relational_expression()
    {
        auto result = parse_shift_expression();
        for (;;)
        {
            pooled_object<relational_expression, base> e;
            switch (lex.peek().type)
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
            do_implicit_conversion(e);
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
                e = make_pooled_object<right_shift>();
            }
            break;
            default:
                return result;
            }
            lex.next();
            e->lhs = e.merge(result);
            e->rhs = e.merge(parse_additive_expression());
            do_implicit_conversion(e);
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
            do_implicit_conversion(e);
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
            do_implicit_conversion(e);
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
            pooled_object<explicit_cast, base> e = make_pooled_object<explicit_cast>();
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
                pooled_object<addition, base> arithPlus = make_pooled_object<addition>();
                arithPlus->lhs = arithPlus.merge(result);
                arithPlus->rhs = arithPlus.merge(subscriptor);
                do_implicit_conversion(arithPlus);
                pooled_object<indirection, base> deref = make_pooled_object<indirection>();
                deref->expr = deref.merge(arithPlus);
                result = std::move(deref);
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
                return make_pooled_object<signed_int>();
            case token_type::type_char:
                lex.next();
                return make_pooled_object<signed_char>();
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
                return make_pooled_object<unsigned_int>();
            case token_type::type_char:
                lex.next();
                return make_pooled_object<unsigned_char>();
            }
        }
        break;
        case token_type::type_int:
        {
            lex.next();
            return make_pooled_object<signed_int>();
        }
        case token_type::type_char:
        {
            lex.next();
            return make_pooled_object<plain_char>();
        }
        case token_type::type_void:
        {
            lex.next();
            return make_pooled_object<void_type>();
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

    template<bool IsIdentifierRequired>
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
            decl = parse_declaration_or_type<IsIdentifierRequired>();
            decl->set_innermost_type(decl.merge(type));
        }
        break;
        case token_type::asterisk:
        {
            lex.next();
            decl = parse_declaration_or_type<IsIdentifierRequired>();
            decl->set_innermost_type(decl.emplace<pointer_type>());
        }
        break;
        case token_type::open_parentheses:
        {
            lex.next();
            decl = parse_declaration_or_type<IsIdentifierRequired>();
            expect(token_type::close_parentheses);
        }
        break;
        case token_type::identifier:
        {
            if (!IsIdentifierRequired)
            {
                error("Unexpected identifier in parsing a type.", lex.peek());
            }
            decl = make_pooled_object<declaration>(lex.next().content);
        }
        break;
        default://may be just a type, allow it
        {
            if (IsIdentifierRequired)
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
