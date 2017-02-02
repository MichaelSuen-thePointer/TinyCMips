#pragma once
#include <utility>
#include <iterator>
#include <cassert>
#include <string>

namespace mq
{

template<class TCont>
struct char32_iterator_traits
{
    using container_type = const TCont;
    using iterator_type = decltype(std::cbegin(std::declval<container_type>()));
    using iterator_category = std::forward_iterator_tag;
    using value_type = char32_t;
    using reference = const value_type&;
    using pointer = const value_type*;
    using difference_type = ptrdiff_t;
};

template<class TTraits>
class char32_iterator_base
{
public:
    using container_type = typename TTraits::container_type;
    using iterator_type = typename TTraits::iterator_type;
    using iterator_category = typename TTraits::iterator_category;
    using value_type = typename TTraits::value_type;
    using reference = typename TTraits::reference;
    using pointer = typename TTraits::pointer;
    using difference_type = typename TTraits::difference_type;

protected:
    char32_iterator_base()
        : char32_iterator_base({}, {}, {}, {})
    {
    }

    char32_iterator_base(const container_type* cont, iterator_type curr, iterator_type next, value_type ch)
        : _ref{cont}
        , _curr{curr}
        , _next{next}
        , _ch{ch}
    {
    }
public:
    reference operator*() const
    {
        assert(!is_end());
        return _ch;
    }

    friend bool operator==(const char32_iterator_base& lhs, const char32_iterator_base& rhs)
    {
        if (lhs.is_end() || rhs.is_end())
        {
            return lhs.is_end() && rhs.is_end();
        }
        assert(lhs._ref == rhs._ref);
        return lhs._curr == rhs._curr;
    }

    friend bool operator!=(const char32_iterator_base& lhs, const char32_iterator_base& rhs)
    {
        return !(lhs == rhs);
    }
protected:
    bool is_end() const
    {
        return _curr == _next;
    }

    container_type* _ref;
    iterator_type _curr;
    iterator_type _next;
    char32_t _ch;
};


template<class TCont>
class unchecked_char32_iterator : public char32_iterator_base<char32_iterator_traits<const TCont>>
{
    using base = char32_iterator_base<char32_iterator_traits<const TCont>>;
public:
    using typename base::reference;
    using typename base::container_type;
    using typename base::iterator_type;
    using typename base::iterator_category;
    using typename base::value_type;
    using typename base::pointer;
    using typename base::difference_type;
protected:
    explicit unchecked_char32_iterator(const container_type* cont, iterator_type curr, iterator_type next, value_type ch)
        : base(cont, curr, next, ch)
    {
    }
public:
    unchecked_char32_iterator()
        : base{}
    {
    }

    explicit unchecked_char32_iterator(const TCont& cont)
        : base{&cont, std::begin(cont), std::begin(cont), {}}
    {
        get_char();
    }

    reference operator*() const
    {
        return base::operator*();
    }

    unchecked_char32_iterator& operator++()
    {
        assert(!this->is_end());
        get_char();
        return *this;
    }

    unchecked_char32_iterator operator++(int)
    {
        auto copy = *this;
        ++(*this);
        return copy;
    }
private:
    void get_char()
    {
        if (this->_next == std::cend(*(this->_ref)))
        {
            this->_curr = this->_next;
            return;
        }
        this->_curr = this->_next;
        unsigned char c = *(this->_next)++;
        if (c < 0b1000'0000u) //单码点
        {
            this->_ch = c;
            return;
        }
        int len = 0;
        assert(!(c < 0b11000000u || c > 0b1111'1110u)); //[1000'0000, 1100'0000) U [1111'1110,)无效
        if (c < 0b1110'0000u) //[1100'0000, 1110'0000)2字节
        {
            c &= 0b0001'1111u;
            len = 2;
        }
        else if (c < 0b1111'0000u) //[1110'0000, 1111'0000)3字节
        {
            c &= 0b0000'1111u;
            len = 3;
        }
        else if (c < 0b1111'1000u) //[1111'0000, 1111'1000)4字节
        {
            c &= 0b0000'0111u;
            len = 4;
        }
        else if (c < 0b1111'1100u) //[1111'1100, 1111'1100)5字节
        {
            c &= 0b0000'0011u;
            len = 5;
        }
        else if (c < 0b1111'1110u) //[1111'1100, 1111'1110)6字节
        {
            c &= 0b0000'0001u;
            len = 6;
        }
        else
        {
            assert(("cannot be here", 0));
        }
        char32_t ch = c;
        assert(std::cend(*(this->_ref)) - this->_next < len - 1);
        for (int i = 1; i < len; i++)
        {
            c = *(this->_next)++;
            assert(!(((c & 0b1100'0000u) != 0b1000'0000u)));
            ch = (ch << 6) | (c & 0b0011'1111);
        }
        //apply changes
        this->_ch = ch;
    }
};

template<class TCont>
class char32_iterator : public unchecked_char32_iterator<TCont>
{
    using base = unchecked_char32_iterator<TCont>;
public:
    using typename base::reference;
    using typename base::container_type;
    using typename base::iterator_type;
    using typename base::iterator_category;
    using typename base::value_type;
    using typename base::pointer;
    using typename base::difference_type;

    char32_iterator()
        : base{}
    {
    }

    explicit char32_iterator(const TCont& cont)
        : base{&cont, std::begin(cont), std::begin(cont), {}}
    {
        get_char();
    }

    reference operator*() const
    {
        return base::operator*();
    }

    char32_iterator& operator++()
    {
        assert(!this->is_end());
        get_char();
        return *this;
    }

    char32_iterator operator++(int)
    {
        auto copy = *this;
        ++(*this);
        return copy;
    }
private:
    [[noreturn]]
    static void bad_char(char32_t ch)
    {
        throw std::runtime_error("bad utf-8 character");
    }

    void get_char()
    {
        if (this->_next == std::cend(*(this->_ref)))
        {
            this->_curr = this->_next;
            return;
        }
        auto curr = this->_next;
        auto next = this->_next;
        unsigned char c = *next++;
        char32_t ch = 0;
        if (c > 0b0111'1111) //不是单码点
        {
            if (c < 0b11000000u || c > 0b1111'1110u) //[1000'0000, 1100'0000) U [1111'1110,)无效
            {
                bad_char(c);
            }
            int len = 0;
            if (c < 0b1110'0000u) //[1100'0000, 1110'0000)2字节
            {
                c &= 0b0001'1111u;
                len = 2;
            }
            else if (c < 0b1111'0000u) //[1110'0000, 1111'0000)3字节
            {
                c &= 0b0000'1111u;
                len = 3;
            }
            else if (c < 0b1111'1000u) //[1111'0000, 1111'1000)4字节
            {
                c &= 0b0000'0111u;
                len = 4;
            }
            else if (c < 0b1111'1100u) //[1111'1100, 1111'1100)5字节
            {
                c &= 0b0000'0011u;
                len = 5;
            }
            else if (c < 0b1111'1110u) //[1111'1100, 1111'1110)6字节
            {
                c &= 0b0000'0001u;
                len = 6;
            }
            else
            {
                assert(("cannot be here", 0));
            }
            ch = c;
            if (std::cend(*(this->_ref)) - next < len - 1)
            {
                bad_char(ch);
            }
            for (int i = 1; i < len; i++)
            {
                c = *next++;
                ch = (ch << 6) | (c & 0b0011'1111u);
                if ((c & 0b1100'0000u) != 0b1000'0000u)
                {
                    bad_char(ch);
                }
            }
        }
        //apply changes
        this->_ch = ch;
        this->_curr = curr;
        this->_next = next;
    }
};

template<class TCont, class = decltype(std::cbegin(std::declval<TCont>()))>
auto make_char32_iterator(const TCont& cont)
{
    return char32_iterator<TCont>(cont);
}

template<class TCont, class = decltype(std::cbegin(std::declval<TCont>()))>
auto make_unchecked_char32_iterator(const TCont& cont)
{
    return unchecked_char32_iterator<TCont>(cont);
}

}