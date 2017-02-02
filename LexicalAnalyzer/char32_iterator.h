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
    using iterator_category = std::input_iterator_tag;
    using value_type = char32_t;
    using reference = value_type;
    using pointer = const value_type*;
    using difference_type = ptrdiff_t;
};

template<class TIter>
class char32_iterator_base
{
public:
    using iterator_type = TIter;
    using iterator_category = std::input_iterator_tag;
    using value_type = char32_t;
    using reference = value_type;
    using pointer = const value_type*;
    using difference_type = ptrdiff_t;

protected:
    char32_iterator_base(iterator_type iter)
        : _curr{iter}
    {
    }
public:

    friend bool operator==(const char32_iterator_base& lhs, const char32_iterator_base& rhs)
    {
        return lhs._curr == rhs._curr;
    }

    friend bool operator!=(const char32_iterator_base& lhs, const char32_iterator_base& rhs)
    {
        return !(lhs == rhs);
    }
protected:
    iterator_type _curr;
};


template<class TIter>
class char32_iterator : public char32_iterator_base<TIter>
{
    using base = char32_iterator_base<TIter>;
public:
    using typename base::reference;
    using typename base::iterator_type;
    using typename base::iterator_category;
    using typename base::value_type;
    using typename base::pointer;
    using typename base::difference_type;

    explicit char32_iterator(iterator_type iter)
        : base(iter)
    {
    }

    char32_t operator*() const
    {
        return get_char();
    }

    char32_iterator& operator++()
    {
        next_char();
        return *this;
    }

    char32_iterator operator++(int)
    {
        auto copy = *this;
        ++(*this);
        return copy;
    }
private:
    void next_char()
    {
        this->_curr += calc_length();
    }
    int calc_length() const
    {
        auto copy = this->_curr;
        unsigned char c = *copy;
        if (c < 0b1000'0000u) //单码点
        {
            return 1;
        }
        if (c < 0b1110'0000u) //[1100'0000, 1110'0000)2字节
        {
            return 2;
        }
        if (c < 0b1111'0000u) //[1110'0000, 1111'0000)3字节
        {
            return 3;
        }
        if (c < 0b1111'1000u) //[1111'0000, 1111'1000)4字节
        {
            return 4;
        }
        if (c < 0b1111'1100u) //[1111'1100, 1111'1100)5字节
        {
            return 5;
        }
        if (c < 0b1111'1110u) //[1111'1100, 1111'1110)6字节
        {
            return 6;
        }
        assert(0);
        return 0;
    }
    char32_t get_char() const
    {
        auto len = calc_length();
        char32_t ch = *this->_curr;
        for (int i = 1; i < len; i++)
        {
            assert(!(((c & 0b1100'0000u) != 0b1000'0000u)));
            ch = (ch << 6) | (this->_curr[i] & 0b0011'1111u);
        }
        //apply changes
        return ch;
    }
};

template<class TIter>
auto make_char32_iterator(TIter it)
{
    return char32_iterator<TIter>(it);
}


}