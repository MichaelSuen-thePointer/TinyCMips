#pragma once
#include <memory>
#include <vector>
#include <iostream>
namespace mq
{

template<class T, class TBase = T>
class pooled_object : public std::unique_ptr<T>
{
public:
    using base = std::unique_ptr<T>;
    using self = pooled_object;
    using typename base::deleter_type;
    using typename base::element_type;
    using typename base::pointer;
    using base::get_deleter;
    using base::get;
    using base::operator->;
    using base::operator*;
    using base::operator bool;

    constexpr pooled_object() noexcept
        : base(pointer())
    {
    }

    constexpr pooled_object(nullptr_t) noexcept
        : base(pointer())
    {
    }

    explicit pooled_object(pointer p) noexcept
        : base(p)
    {
    }

    pooled_object(std::unique_ptr<T>&& p) noexcept
        : base(std::move(p))
    {
    }

    pooled_object(pooled_object&& right) noexcept
        : base(static_cast<base&&>(right))
        , _pool(std::move(right._pool))
    {
    }

    template<class T2, class TBase2>
    pooled_object(pooled_object<T2, TBase2>&& right) noexcept
        : base(static_cast<typename pooled_object<T2, TBase2>::base&&>(right))
        , _pool(std::make_move_iterator(right._pool.begin()), std::make_move_iterator(right._pool.end()))
    {
    }

    pooled_object& operator=(pooled_object&& right) noexcept
    {
        base::operator=(static_cast<base&&>(right));
        _pool = std::move(right._pool);
        return *this;
    }

    template<class T2, class TBase2>
    pooled_object& operator=(pooled_object<T2, TBase2>&& right) noexcept
    {
        base::operator=(static_cast<typename pooled_object<T2, TBase2>::base&&>(right));
        _pool.assign(std::make_move_iterator(right._pool.begin()), std::make_move_iterator(right._pool.end()));
        return *this;
    }

    self& operator=(nullptr_t) noexcept
    {
        this->reset();
        return *this;
    }

    pooled_object(const pooled_object&) = delete;
    pooled_object& operator=(const pooled_object&) = delete;

    pointer push(pointer ty)
    {
        _pool.emplace_back(ty);
        return ty;
    }

    template<class T2, class = std::enable_if_t<std::is_convertible<T2*, typename std::unique_ptr<TBase>::pointer>::value>>
    T2 push(T2* ptr)
    {
        _pool.emplace_back(ptr);
        return ptr;
    }

    template<class T2, class D2>
    typename std::unique_ptr<T2>::pointer
        push(std::unique_ptr<T2>&& ptr)
    {
        auto p = ptr.get();
        _pool.emplace_back(std::move(ptr));
        return p;
    }

    pointer push(std::unique_ptr<T>&& ptr)
    {
        _pool.emplace_back(std::move(ptr));
        return _pool.back().get();
    }

    template<class T2, class... Args>
    auto emplace(Args&&... args)
        -> typename std::unique_ptr<T2>::pointer
    {
        auto n = std::make_unique<T2>(std::forward<Args>(args)...);
        auto p = n.get();
        _pool.emplace_back(std::move(n));
        return p;
    }

    template<class T2>
    typename std::unique_ptr<T2>::pointer
        adopt(std::unique_ptr<T2>& ptr)
    {
        auto p = ptr.get();
        _pool.emplace_back(ptr.release());
        return p;
    }

    pointer adopt(std::unique_ptr<T>& ptr)
    {
        _pool.emplace_back(ptr.release());
        return _pool.back().get();
    }

    pointer merge(pooled_object&& obj)
    {
        return merge(obj);
    }

    template<class T2, class TBase2>
    typename pooled_object<T2, TBase2>::pointer
        merge(pooled_object<T2, TBase2>&& obj)
    {
        return merge(obj);
    }


    pointer merge(pooled_object& obj)
    {
        pointer p = obj.release();
        _pool.reserve(_pool.size() + obj._pool.size() + 1);
        _pool.insert(_pool.end(),
                     std::make_move_iterator(obj._pool.begin()),
                     std::make_move_iterator(obj._pool.end()));
        _pool.emplace_back(p);
        obj._pool.clear();
        return p;
    }

    template<class T2, class TBase2>
    typename pooled_object<T2, TBase2>::pointer
        merge(pooled_object<T2, TBase2>& obj)
    {
        auto p = obj.release();
        _pool.reserve(_pool.size() + obj._pool.size() + 1);
        _pool.insert(_pool.end(),
                     std::make_move_iterator(obj._pool.begin()),
                     std::make_move_iterator(obj._pool.end()));
        _pool.emplace_back(p);
        obj._pool.clear();
        return p;
    }

    template<class Ptr>
    Ptr* as() noexcept
    {
        return reinterpret_cast<Ptr*>(this->get());
    }

    void reset()
    {
        base::reset();
        _pool.clear();
    }

    std::vector<std::unique_ptr<TBase>>& pool() { return _pool; }

    const std::vector<std::unique_ptr<TBase>>& pool() const { return _pool; }

    std::vector<std::unique_ptr<TBase>> _pool;
};

template<class T, class... Args>
pooled_object<T> make_pooled_object(Args&&... args)
{
    return pooled_object<T>{std::make_unique<T>(std::forward<Args>(args)...)};
}


}