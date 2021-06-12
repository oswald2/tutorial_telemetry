


#ifndef SLICE_HPP
#define SLICE_HPP

#include <iterator>
#include <memory>

namespace Util
{


  /// \brief Represents a read/writable slice into a buffer. Supports iterators and can 
  ///        therefore be used in C++11 style loops
  template<typename T>
  class slice
  {
    public:

      typedef T value_type;
      typedef value_type& reference;
      typedef const value_type& const_reference;
      typedef value_type* pointer;
      typedef const value_type* const_pointer;
      typedef T* iterator;
      typedef const T* const_iterator;
      typedef std::size_t size_type;
      typedef std::ptrdiff_t difference_type;

      slice() : m_ptr(nullptr), m_size(0)
      {}

      slice(T* ptr, std::size_t size) : m_ptr(ptr), m_size(size)
      {}

      slice(const slice& rhs) : m_ptr(rhs.m_ptr), m_size(rhs.m_size)
      {}

      slice(slice&& rhs) : m_ptr(std::move(rhs.m_ptr)), m_size(std::move(rhs.m_size))
      {}

      bool empty() const noexcept
      {
        return m_size == 0;
      }

      size_type size() const noexcept
      {
        return m_size;
      }

      pointer data() const noexcept
      {
        return m_ptr;
      }

      iterator begin() noexcept
      {
        return m_ptr;
      }
      const_iterator begin() const noexcept
      {
        return m_ptr;
      }
      const_iterator cbegin() const noexcept
      {
        return m_ptr;
      }

      iterator end() noexcept
      {
        return m_ptr + m_size;
      }
      const_iterator end() const noexcept 
      {
        return m_ptr + m_size;
      }
      const_iterator cend() const noexcept 
      {
        return m_ptr + m_size;
      }

      reference operator[](size_type n)
      {
        return m_ptr[n];
      }
      const_reference operator[](size_type n) const
      {
        return m_ptr[n];
      }

      reference at(size_type n)
      {
        if(n >= m_size) throw std::out_of_range("slice out of range check");
        return m_ptr[n];
      }
      const_reference at(size_type n) const
      {
        if(n >= m_size) throw std::out_of_range("slice out of range check");
        return m_ptr[n];
      }

      reference front() 
      {
        return *m_ptr;
      }
      const_reference front() const 
      {
        return *m_ptr;
      }

      reference back() 
      {
        return m_ptr[m_size - 1]; 
      }
      const_reference back() const 
      {
        return m_ptr[m_size - 1];
      }

    private:
      T* m_ptr;
      std::size_t m_size;
  };


  /// \brief Represents a readonly slice into a buffer. Supports iterators and can 
  ///        therefore be used in C++11 style loops
  template<typename T>
  class const_slice
  {
    public:

      typedef T value_type;
      typedef const value_type& const_reference;
      typedef const value_type* const_pointer;
      typedef const T* const_iterator;
      typedef std::size_t size_type;
      typedef std::ptrdiff_t difference_type;

      const_slice() : m_ptr(nullptr), m_size(0)
      {}

      const_slice(const T* ptr, std::size_t size) : m_ptr(ptr), m_size(size)
      {}

      const_slice(const const_slice& rhs) : m_ptr(rhs.m_ptr), m_size(rhs.m_size)
      {}

      const_slice(const slice<T>& rhs) : m_ptr(rhs.data()), m_size(rhs.size())
      {}

      const_slice(const_slice&& rhs) : m_ptr(std::move(rhs.m_ptr)), m_size(std::move(rhs.m_size))
      {}

      bool empty() const noexcept
      {
        return m_size == 0;
      }

      size_type size() const noexcept
      {
        return m_size;
      }

      const_pointer data() const noexcept
      {
        return m_ptr;
      }

      const_iterator begin() const noexcept
      {
        return m_ptr;
      }
      const_iterator cbegin() const noexcept
      {
        return m_ptr;
      }

      const_iterator end() const noexcept 
      {
        return m_ptr + m_size;
      }
      const_iterator cend() const noexcept 
      {
        return m_ptr + m_size;
      }

      const_reference operator[](size_type n) const
      {
        return m_ptr[n];
      }

      const_reference at(size_type n) const
      {
        if(n >= m_size) throw std::out_of_range("slice out of range check");
        return m_ptr[n];
      }

      const_reference front() const 
      {
        return *m_ptr;
      }

      const_reference back() const 
      {
        return m_ptr[m_size - 1];
      }

    private:
      const T* m_ptr;
      std::size_t m_size;
  };

  /// \brief Helper function to create a slice from a pointer to a buffer and a size
  template<typename T>
  slice<T> make_slice(T* ptr, std::size_t size)
  {
    return slice<T>(ptr, size);
  }

  /// \brief Helper function to create a const_slice from a pointer to a buffer and a size
  template<typename T>
  const_slice<T> make_const_slice(const T* ptr, std::size_t size)
  {
    return const_slice<T>(ptr, size);
  }


}



#endif // !SLICE_HPP