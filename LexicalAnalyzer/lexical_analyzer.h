#pragma once
#include "char32_iterator.h"
#include <vector>
#include <sstream>
#include <map>
#include <locale>
#include <codecvt>

namespace mq
{

class lexical_error : public std::runtime_error
{
    int line, column;
public:
    explicit lexical_error(const std::string& _message, int line, int column)
        : runtime_error(make_error_msg(_message, line, column))
        , line(line), column(column)
    {
    }

private:
    static std::string make_error_msg(const std::string& msg, int line, int column)
    {
        std::ostringstream errmsg;
        errmsg << msg << " At line: " << line << " column: " << column;
        return errmsg.str();
    }
};

enum class token_type
{
    none,
    keyword_if,
    keyword_else,
    keyword_while,
    keyword_break,
    keyword_continue,
    keyword_return,
    type_int,
    type_char,
    type_void,
    type_unsigned,
    type_signed,
    identifier,

    number_literal,
    char_literal,
    string_literal,

    size_of,
    asterisk,
    dot,
    arrow,
    plus,
    inc, //++
    dec, //--
    sub,
    mul,
    div,
    mod,
    shl,
    shr,
    bit_and,
    bit_or,
    and_,
    or_ ,
    xor_,
    bit_not,
    not_,
    assign,
    eq,
    gt,
    lt,
    ge,
    le,
    ne,
    comma,
    semicolon,
    colon,
    question_mark,
    open_parentheses, //(
    close_parentheses, //(
    open_bracket, //[
    close_bracket, //[
    open_brace, //{
    close_brace //{
};

#ifdef _DEBUG

const char* token_type_strs[] =
{
    "none",
    "keyword_if",
    "keyword_else",
    "keyword_while",
    "keyword_break",
    "keyword_continue",
    "keyword_return",
    "type_int",
    "type_char",
    "type_void",
    "type_unsigned",
    "type_signed",
    "identifier",
    "number_literal",
    "char_literal",
    "string_literal",
    "size_of",
    "asterisk",
    "dot",
    "arrow",
    "plus",
    "inc", //++
    "dec", //--
    "sub",
    "mul",
    "div",
    "mod",
    "shl",
    "shr",
    "bit_and",
    "bit_or",
    "and_",
    "or_ ",
    "xor_",
    "bit_not",
    "not_",
    "assign",
    "eq",
    "gt",
    "lt",
    "ge",
    "le",
    "ne",
    "comma",
    "semicolon",
    "colon",
    "question_mark",
    "open_parentheses", //(
    "close_parentheses", //(
    "open_bracket", //[
    "close_bracket", //[
    "open_brace", //{
    "close_brace" //{
};

#endif

struct code_token
{
    std::u32string content;
    token_type type;
    int line;
    int column;

    code_token()
        : code_token(U"", token_type::none, -1, -1)
    {
    }

    code_token(const std::u32string& content, token_type type, int line, int column)
        : content(content)
        , type(type)
        , line(line)
        , column(column)
    {
    }

    friend std::ostream& operator<<(std::ostream& os, const code_token& tok)
    {
        using fuck_msvc_char32_t = unsigned __int32;
        std::wstring_convert<std::codecvt_utf8<fuck_msvc_char32_t>, fuck_msvc_char32_t> utf32to8;
        auto fuckMSVCStr = reinterpret_cast<const fuck_msvc_char32_t*>(tok.content.data());

        os << "[token]" << utf32to8.to_bytes(fuckMSVCStr, fuckMSVCStr + tok.content.size()) << ' ';
#ifdef _DEBUG
        os << token_type_strs[(std::underlying_type_t<token_type>)tok.type] << ' ';
#else
        os << tok.type << ' ';
#endif
        os << tok.line << '(' << tok.column << ')';
        return os;
    }
};

static const std::map<std::u32string, token_type> keywords
{
    {U"if", token_type::keyword_if},
    {U"else", token_type::keyword_else},
    {U"sizeof", token_type::size_of},
    {U"while", token_type::keyword_while},
    {U"continue", token_type::keyword_continue},
    {U"return", token_type::keyword_return},
    {U"break", token_type::keyword_break},
    {U"void", token_type::type_void},
    {U"char", token_type::type_char},
    {U"int", token_type::type_int},
    {U"signed", token_type::type_signed},
    {U"unsigned", token_type::type_unsigned}
};

class lexical_analyzer
{
    std::string _content;
    struct parsing_state
    {
        int line = 1;
        int column = 1;
        int anchor_line;
        int anchor_column;
        enum state
        {
            none,
            in_dot,
            in_identifier_keyword,
            in_type,
            in_comment,
        } state = none;
        char32_iterator<std::string::const_iterator> anchor;
    private:
        char32_iterator<std::string::const_iterator> _curr;
    public:
        char32_iterator<std::string::const_iterator> end;
        explicit operator bool() const { return _curr != end; }
        char32_t operator*() { return *_curr; }

        const char32_iterator<std::string::const_iterator>& curr() const
        {
            return _curr;
        }

        void curr(const char32_iterator<std::string::const_iterator>& curr)
        {
            _curr = curr;
            check_new_line();
        }

        parsing_state(const std::string& content)
            : parsing_state(1, 1, 1, 1, none,
                            make_char32_iterator(content.end()),
                            make_char32_iterator(content.begin()),
                            make_char32_iterator(content.end()))
        {
        }

        parsing_state(int line, int column, int anchorLine, int anchorColumn, enum state state, const char32_iterator<std::string::const_iterator>& anchor, const char32_iterator<std::string::const_iterator>& curr, const char32_iterator<std::string::const_iterator>& end)
            : line(line)
            , column(column)
            , anchor_line(anchorLine)
            , anchor_column(anchorColumn)
            , state(state)
            , anchor(anchor)
            , _curr(curr)
            , end(end)
        {
        }

        parsing_state& operator++()
        {
            ++_curr;
            ++column;
            check_new_line();
            return *this;
        }
        void set_anchor()
        {
            anchor = _curr;
            anchor_line = line;
            anchor_column = column;
        }
    private:
        void check_new_line()
        {
            if (_curr != end && *_curr == '\r')
            {
                ++line;
                column = 1;
                ++_curr;
                if (_curr != end && *_curr == '\n')
                {
                    ++_curr;
                }
                if (state == in_comment)
                {
                    state = none;
                }
            }
            else if (_curr != end && *_curr == '\n')
            {
                ++line;
                column = 1;
                ++_curr;
                if (state == in_comment)
                {
                    state = none;
                }
            }
        }
    } _state;

    code_token _current;
public:
    lexical_analyzer(const std::string& content)
        : _content{content + ' '}
        , _state(_content)
    {
        reset();
    }

    void reset()
    {
        _state.curr(make_char32_iterator(_content.cbegin()));
    }

    void initialize()
    {
        _current = next_token();
    }

    code_token next()
    {
        code_token ret = _current;
        _current = next_token();
        return ret;
    }

    code_token peek() const
    {
        return _current;
    }
private:
#define DIGIT '0':case'1':case'2':case'3':case'4':case'5':case'6':case'7':case'8':case'9'
#define HEX_DIGIT DIGIT:case'A':case'B':case'C':case'D':case'E':case'F':case'a':case'b':case'c':case'd':case'e':case'f'
#define SPACE ' ':case'\r':case'\n':case'\t'
#define OCT_DIGIT '0':case'1':case'2':case'3':case'4':case'5':case'6':case'7'
#define NON_DOT_PUNCT '~':case'!':case'%':case'^':case'&':case'*':case'(':case')':case'+':case'-':case'=':case'{':case'}':case'[':case']':case':':case'\\':case'|':case'\"':case'\'':case';':case'?':case'>':case'<':case'/':case','
#define PUNCT NON_DOT_PUNCT:case'.'
    code_token next_token()
    {
        while (_state)
        {
            switch (_state.state)
            {
            case parsing_state::none:
                switch (*_state)
                {
                case SPACE:
                    ++_state;
                    break;
                case DIGIT:
                    return parse_number();
                case '>':
                    return parse_token_if_follows_else('=', token_type::ge, token_type::gt);
                case '<':
                    return parse_token_if_follows_else('=', token_type::le, token_type::lt);
                case '!':
                    return parse_token_if_follows_else('=', token_type::ne, token_type::not_);
                case '=':
                    return parse_token_if_follows_else('=', token_type::eq, token_type::assign);
                case '&':
                    return parse_token_if_follows_else('&', token_type::and_, token_type::bit_and);
                case '|':
                    return parse_token_if_follows_else('|', token_type:: or_ , token_type::bit_or);
                case '+':
                    return parse_token_if_follows_else('+', token_type::inc, token_type::plus);
                case '-':
                    _state.set_anchor();
                    ++_state;
                    if (*_state == '>')
                    {
                        return make_token(token_type::arrow);
                    }
                    if (*_state == '-')
                    {
                        return make_token(token_type::dec);
                    }
                case '[':
                    _state.set_anchor();
                    ++_state;
                    return make_token(token_type::open_bracket);
                case ']':
                    _state.set_anchor();
                    ++_state;
                    return make_token(token_type::close_bracket);
                case '(':
                    _state.set_anchor();
                    ++_state;
                    return make_token(token_type::open_parentheses);
                case ')':
                    _state.set_anchor();
                    ++_state;
                    return make_token(token_type::close_parentheses);
                case '{':
                    _state.set_anchor();
                    ++_state;
                    return make_token(token_type::open_brace);
                case '}':
                    _state.set_anchor();
                    ++_state;
                    return make_token(token_type::close_brace);
                case '?':
                    _state.set_anchor();
                    ++_state;
                    return make_token(token_type::question_mark);
                case ';':
                    _state.set_anchor();
                    ++_state;
                    return make_token(token_type::semicolon);
                case ':':
                    _state.set_anchor();
                    ++_state;
                    return make_token(token_type::colon);
                case ',':
                    _state.set_anchor();
                    ++_state;
                    return make_token(token_type::comma);
                case '/':
                    _state.set_anchor();
                    ++_state;
                    if (*_state == '/')
                    {
                        ++_state;
                        _state.state = parsing_state::in_comment;
                    }
                    else
                    {
                        return make_token(token_type::div);
                    }
                case '*':
                    _state.set_anchor();
                    ++_state;
                    return make_token(token_type::asterisk);
                case '%':
                    _state.set_anchor();
                    ++_state;
                    return make_token(token_type::mod);
                case '^':
                    _state.set_anchor();
                    ++_state;
                    return make_token(token_type::xor_);
                case '~':
                    _state.set_anchor();
                    ++_state;
                    return make_token(token_type::bit_not);
                case '\"':
                    return parse_string_literal();
                case '\'':
                    return parse_char_literal();
                case '.':
                    ++_state;
                    _state.state = parsing_state::in_dot;
                    break;
                default:
                    if (is_identifier_keyword_start_char(*_state))
                    {
                        _state.set_anchor();
                        ++_state;
                        _state.state = parsing_state::in_identifier_keyword;
                    }
                    else
                    {
                        unexpected_char();
                    }
                    break;
                }
                break;
            case parsing_state::in_dot:
                switch (*_state)
                {
                case DIGIT:
                    floating_point_not_supported();
                default:
                    _state.state = parsing_state::none;
                    break;
                }
                break;
            case parsing_state::in_identifier_keyword:
                if (is_identifier_keyword_middle_char(*_state))
                {
                    ++_state;
                    break;
                }
                else
                {
                    _state.state = parsing_state::none;
                    return distingush_identifier_keyword();
                }
            case parsing_state::in_comment:
                while (_state.state == parsing_state::in_comment)
                {
                    ++_state;
                }
                break;
            default:
                unexpected_char();
            }
        }
        return{};
    }

private:
    code_token parse_string_literal()
    {
        enum
        {
            none,
        } state = none;
        _state.set_anchor();
        ++_state; //accept \"
        while (*_state)
        {
            switch (state)
            {
            case none:
                switch (*_state)
                {
                case '\\':
                    ++_state;
                    check_escape_sequence();
                    break;
                case '\"':
                    ++_state;
                    return make_string_literal_token();
                case '\r':case '\n':
                    error("String literal not ended with \"", _state.anchor_line, _state.anchor_column);
                default:
                    ++_state;
                }
                break;
            default:
                unexpected_char();
            }
        }
        unexpected_char();
    }
    void check_escape_sequence()
    {
        switch (*_state)
        {
        case'\'':case'\"':case'?':case'\\':case'a':
        case'r':case't':case'v':case'b':case'f':case'n':
            ++_state;
            break;
        case'x':
            ++_state;
            for (int i = 0; i < 2; i++)
            {
                switch (*_state)
                {
                case HEX_DIGIT:
                    ++_state;
                    break;
                default:
                    bad_escape_sequence();
                }
            }
            break;
        case OCT_DIGIT:
            ++_state;
            for (int i = 0; i < 2; i++)
            {
                switch (*_state)
                {
                case OCT_DIGIT:
                    ++_state;
                    break;
                default:
                    bad_escape_sequence();
                }
            }
            break;
        default:
            bad_escape_sequence();
        }
    }
    code_token parse_char_literal()
    {
        _state.set_anchor();
        ++_state;
        switch (*_state)
        {
        case '\\':
            ++_state;
            check_escape_sequence();
            if (*_state == '\'')
            {
                ++_state;
                return make_char_literal_token();
            }
        default:
            ++_state;
            if (*_state == '\'')
            {
                ++_state;
                return make_char_literal_token();
            }
            error("Char literal not closeed", _state.anchor_line, _state.anchor_column);
        }
    }
    code_token distingush_identifier_keyword()
    {
        std::u32string content{_state.anchor, _state.curr()};
        auto result = keywords.find(content);
        if (result != keywords.end())
        {
            return make_token(result->second); //keyword
        }
        return make_token(content); //identifier
    }
    code_token make_char_literal_token()
    {
        std::u32string content{1,'\0'};

        switch (*++_state.anchor)
        {
        case '\\':
            switch (*++_state.anchor)
            {
            case'\'':case'\"':case'?':case'\\':
                content[0] = *_state.anchor;
                break;
            case'a':
                content[0] = '\a';
                break;
            case'r':
                content[0] = '\r';
                break;
            case't':
                content[0] = '\t';
                break;
            case'v':
                content[0] = '\v';
                break;
            case'b':
                content[0] = '\b';
                break;
            case'f':
                content[0] = '\f';
                break;
            case'n':
                content[0] = '\n';
                break;
            case'x':
                content[0] = hex_to_dec(*++_state.anchor);
                content[0] = content[0] * 16 + hex_to_dec(*++_state.anchor);
                break;
            case OCT_DIGIT:
                content[0] = *++_state.anchor - '0';
                content[0] = content[0] * 8 + *++_state.anchor - '0';
                content[0] = content[0] * 8 + *++_state.anchor - '0';
                break;
            default:
                assert(!!"can never be here");
            }
        default:
            content[0] = *_state.anchor;
        }
        return code_token{content, token_type::char_literal, _state.anchor_line, _state.anchor_column};
    }
    static char32_t translate_escape_sequence(char32_iterator<std::string::const_iterator>& iter)
    {
        switch (*++iter)
        {
        case'\'':case'\"':case'?':case'\\':
            return *iter++;
        case'a':
            ++iter;
            return '\a';
        case'r':
            ++iter;
            return '\r';
        case't':
            ++iter;
            return '\t';
        case'v':
            ++iter;
            return '\v';
        case'b':
            ++iter;
            return '\b';
        case'f':
            ++iter;
            return '\f';
        case'n':
            ++iter;
            return '\n';
        case'x':
        {
            char32_t ch = hex_to_dec(*++iter);
            ch = ch * 16 + hex_to_dec(*++iter);
            ++iter;
            return ch;
        }
        case OCT_DIGIT:
        {
            char32_t ch;
            ch = *++iter - '0';
            ch = ch * 8 + *++iter - '0';
            ch = ch * 8 + *++iter - '0';
            ++iter;
            return ch;
        }
        default:
            assert(!!"can never be here");
            return 0;
        }
    }
    static char32_t hex_to_dec(char32_t c)
    {
        if (c >= '0' && c <= '9')
        {
            return c - '0';
        }
        if (c >= 'a' && c <= 'f')
        {
            return c - 'a' + 10;
        }
        return c - 'A' + 10;
    }
    code_token make_string_literal_token()
    {
        std::u32string content;
        ++_state.anchor;
        while (*_state.anchor != '\"')
        {
            if (*_state.anchor == '\\')
            {
                content.push_back(translate_escape_sequence(_state.anchor));
            }
            else
            {
                content.push_back(*_state.anchor);
                ++_state.anchor;
            }
        }
        return{content, token_type::string_literal, _state.anchor_line, _state.anchor_column};
    }
    code_token parse_token_if_follows_else(char32_t ifFollows, token_type thenMakeToken, token_type elseMakeToken)
    {
        _state.set_anchor();
        ++_state;
        if (*_state == ifFollows)
        {
            return make_token(thenMakeToken);
        }
        return make_token(elseMakeToken);
    }
    code_token make_token(token_type type) const //make normal token
    {
        auto token = code_token{U"", type, _state.anchor_line, _state.anchor_column};
        return token;
    }
    code_token make_token(const std::u32string& str) //make identifier
    {
        return code_token{str, token_type::identifier, _state.anchor_line, _state.anchor_column};
    }
    code_token parse_number()
    {
        enum
        {
            none,
            start_zero,
            hex,
            oct,
            dec,
        } state = none;
        while (*_state)
        {
            switch (state)
            {
            case none:
                _state.set_anchor();
                switch (*_state)
                {
                case '0':
                    ++_state;
                    state = start_zero;
                    break;
                default: //must be other digits
                    state = dec;
                }
                break;
            case start_zero:
                switch (*_state)
                {
                case 'x': case 'X':
                    ++_state;
                    state = hex;
                    break;
                case DIGIT:
                    ++_state;
                    state = oct;
                    break;
                case SPACE:case NON_DOT_PUNCT:
                    return code_token{{_state.anchor, _state.curr()}, token_type::number_literal, _state.anchor_line, _state.anchor_column};
                case '.':
                    floating_point_not_supported();
                default:
                    unexpected_char();
                }
                break;
            case hex:
                switch (*_state)
                {
                case HEX_DIGIT:
                    ++_state;
                    break;
                case SPACE:case PUNCT:
                    return code_token{{_state.anchor, _state.curr()}, token_type::number_literal, _state.anchor_line, _state.anchor_column};
                default:
                    unexpected_char();
                }
                break;
            case oct:
                switch (*_state)
                {
                case OCT_DIGIT:
                    ++_state;
                    break;
                case SPACE:case PUNCT:
                    return code_token{{_state.anchor, _state.curr()}, token_type::number_literal, _state.anchor_line, _state.anchor_column};
                default:
                    unexpected_char();
                }
                break;
            case dec:
                switch (*_state)
                {
                case DIGIT:
                    ++_state;
                    break;
                case SPACE:case NON_DOT_PUNCT:
                    return code_token{{_state.anchor, _state.curr()}, token_type::number_literal, _state.anchor_line, _state.anchor_column};
                case '.':
                    floating_point_not_supported();
                default:
                    unexpected_char();
                }
                break;
            default:
                unexpected_char();
            }
        }
        unexpected_char();
    }
    static bool is_identifier_keyword_start_char(char32_t ch)
    {
        if ((ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z'))
        {
            return true;
        }
        if (ch == '_')
        {
            return true;
        }
        if (ch > 0x7FFF'FFFF) //beyond ascii range
        {
            return true;
        }
        return false;
    }
    static bool is_identifier_keyword_middle_char(char32_t ch)
    {
        return is_identifier_keyword_start_char(ch) || (ch >= '0' && ch <= '9');
    }
    [[noreturn]]
    void bad_escape_sequence() const
    {
        error("Bad escaping sequence", _state.line, _state.column);
    }
    [[noreturn]]
    void unexpected_char() const
    {
        error("Unexpected char.", _state.line, _state.column);
    }
    [[noreturn]]
    void floating_point_not_supported() const
    {
        error("Floating point is not supported.", _state.line, _state.column);
    }
    [[noreturn]]
    static void error(const std::string& msg, int line, int column)
    {
        throw lexical_error{msg, line, column};
    }

#undef DIGIT
#undef HEX_DIGIT
#undef OCT_DIGIT
#undef PUNCT
#undef SPACE
};



}
