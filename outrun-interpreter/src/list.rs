//! Linked list implementation for functional programming in Outrun
//!
//! This module provides an efficient linked list implementation optimized for
//! functional programming patterns like head/tail operations, cons, and recursive algorithms.
//!
//! Lists are immutable linked lists (not vectors) as required by Outrun's functional design.

/// Linked list implementation optimized for functional programming
///
/// This structure provides O(1) head/tail operations and efficient sharing
/// of tail references between multiple lists, which is crucial for functional
/// programming patterns in Outrun.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum List<T> {
    /// Empty list
    Empty,
    /// Cons cell containing head value and tail list
    Cons { head: T, tail: Box<List<T>> },
}

impl<T> List<T> {
    /// Create a new empty list
    pub fn new() -> Self {
        List::Empty
    }

    /// Create a new list with a single element
    pub fn singleton(value: T) -> Self {
        List::Cons {
            head: value,
            tail: Box::new(List::Empty),
        }
    }

    /// Cons operation: prepend a value to the front of the list (O(1))
    pub fn cons(head: T, tail: Self) -> Self {
        List::Cons {
            head,
            tail: Box::new(tail),
        }
    }

    /// Get the head (first element) of the list (O(1))
    pub fn head(&self) -> Option<&T> {
        match self {
            List::Empty => None,
            List::Cons { head, .. } => Some(head),
        }
    }

    /// Get the tail (rest of the list) after the first element (O(1))
    pub fn tail(&self) -> Option<&List<T>> {
        match self {
            List::Empty => None,
            List::Cons { tail, .. } => Some(tail),
        }
    }

    /// Check if the list is empty (O(1))
    pub fn is_empty(&self) -> bool {
        matches!(self, List::Empty)
    }

    /// Get the length of the list (O(n))
    pub fn len(&self) -> usize {
        match self {
            List::Empty => 0,
            List::Cons { tail, .. } => 1 + tail.len(),
        }
    }

    /// Prepend a value to the front of the list, returning a new list (O(1))
    /// This is the immutable version of cons that doesn't consume the original list
    pub fn push_front(&self, value: T) -> Self
    where
        T: Clone,
    {
        List::cons(value, self.clone())
    }

    /// Append another list to the end of this list (O(n) where n is length of self)
    pub fn append(&self, other: &List<T>) -> Self
    where
        T: Clone,
    {
        match self {
            List::Empty => other.clone(),
            List::Cons { head, tail } => List::cons(head.clone(), tail.append(other)),
        }
    }

    /// Reverse the list (O(n))
    pub fn reverse(&self) -> Self
    where
        T: Clone,
    {
        self.reverse_helper(List::Empty)
    }

    fn reverse_helper(&self, acc: List<T>) -> List<T>
    where
        T: Clone,
    {
        match self {
            List::Empty => acc,
            List::Cons { head, tail } => tail.reverse_helper(List::cons(head.clone(), acc)),
        }
    }

    /// Convert to a vector for debugging or interop (O(n))
    pub fn to_vec(&self) -> Vec<T>
    where
        T: Clone,
    {
        let mut result = Vec::new();
        let mut current = self;
        while let List::Cons { head, tail } = current {
            result.push(head.clone());
            current = tail;
        }
        result
    }

    /// Create a list from a vector (O(n))
    pub fn from_vec(vec: Vec<T>) -> Self {
        vec.into_iter()
            .rev()
            .fold(List::Empty, |acc, val| List::cons(val, acc))
    }

    /// Map function over the list, returning a new list (O(n))
    pub fn map<U, F>(&self, f: F) -> List<U>
    where
        F: Fn(&T) -> U,
    {
        match self {
            List::Empty => List::Empty,
            List::Cons { head, tail } => List::cons(f(head), tail.map(f)),
        }
    }

    /// Filter the list based on a predicate (O(n))
    pub fn filter<F>(&self, f: F) -> Self
    where
        F: Fn(&T) -> bool + Clone,
        T: Clone,
    {
        match self {
            List::Empty => List::Empty,
            List::Cons { head, tail } => {
                let filtered_tail = tail.filter(f.clone());
                if f(head) {
                    List::cons(head.clone(), filtered_tail)
                } else {
                    filtered_tail
                }
            }
        }
    }

    /// Fold (reduce) the list from the left (O(n))
    pub fn fold_left<U, F>(&self, init: U, f: F) -> U
    where
        F: Fn(U, &T) -> U,
    {
        match self {
            List::Empty => init,
            List::Cons { head, tail } => tail.fold_left(f(init, head), f),
        }
    }

    /// Fold (reduce) the list from the right (O(n))
    /// Note: This implementation converts to vec first for simplicity
    pub fn fold_right<U, F>(&self, init: U, f: F) -> U
    where
        F: Fn(&T, U) -> U,
        T: Clone,
    {
        self.to_vec().iter().rev().fold(init, |acc, x| f(x, acc))
    }

    /// Get element at index (O(n), not recommended for frequent use)
    pub fn get(&self, index: usize) -> Option<&T> {
        if index == 0 {
            self.head()
        } else {
            self.tail()?.get(index - 1)
        }
    }

    /// Take the first n elements (O(n))
    pub fn take(&self, n: usize) -> Self
    where
        T: Clone,
    {
        if n == 0 {
            List::Empty
        } else {
            match self {
                List::Empty => List::Empty,
                List::Cons { head, tail } => List::cons(head.clone(), tail.take(n - 1)),
            }
        }
    }

    /// Drop the first n elements (O(n))
    pub fn drop_front(&self, n: usize) -> Self
    where
        T: Clone,
    {
        if n == 0 {
            self.clone()
        } else {
            match self {
                List::Empty => List::Empty,
                List::Cons { tail, .. } => tail.drop_front(n - 1),
            }
        }
    }

    /// Iterator over the list (for Rust interop)
    pub fn iter(&self) -> ListIterator<T> {
        ListIterator { current: self }
    }
}

impl<T> Default for List<T> {
    fn default() -> Self {
        Self::new()
    }
}

/// Iterator for List
pub struct ListIterator<'a, T> {
    current: &'a List<T>,
}

impl<'a, T> Iterator for ListIterator<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        match self.current {
            List::Empty => None,
            List::Cons { head, tail } => {
                self.current = tail;
                Some(head)
            }
        }
    }
}

impl<T> std::fmt::Display for List<T>
where
    T: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        let mut first = true;
        for value in self.iter() {
            if !first {
                write!(f, ", ")?;
            }
            write!(f, "{}", value)?;
            first = false;
        }
        write!(f, "]")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_list() {
        let list: List<i32> = List::new();
        assert!(list.is_empty());
        assert_eq!(list.len(), 0);
        assert_eq!(list.head(), None);
        assert_eq!(list.tail(), None);
    }

    #[test]
    fn test_singleton_list() {
        let list = List::singleton(42);
        assert!(!list.is_empty());
        assert_eq!(list.len(), 1);
        assert_eq!(list.head(), Some(&42));
        assert!(list.tail().unwrap().is_empty());
    }

    #[test]
    fn test_cons_operations() {
        let empty = List::new();
        let list1 = List::cons(1, empty);
        let list2 = List::cons(2, list1);
        let list3 = List::cons(3, list2);

        assert_eq!(list3.len(), 3);
        assert_eq!(list3.head(), Some(&3));

        let tail1 = list3.tail().unwrap();
        assert_eq!(tail1.head(), Some(&2));

        let tail2 = tail1.tail().unwrap();
        assert_eq!(tail2.head(), Some(&1));
        assert!(tail2.tail().unwrap().is_empty());
    }

    #[test]
    fn test_push_front() {
        let list1 = List::singleton(1);
        let list2 = list1.push_front(2);
        let list3 = list2.push_front(3);

        // Original lists should be unchanged (immutability)
        assert_eq!(list1.len(), 1);
        assert_eq!(list2.len(), 2);
        assert_eq!(list3.len(), 3);

        // Check values
        assert_eq!(list3.head(), Some(&3));
        assert_eq!(list3.tail().unwrap().head(), Some(&2));
        assert_eq!(list3.tail().unwrap().tail().unwrap().head(), Some(&1));
    }

    #[test]
    fn test_append() {
        let list1 = List::from_vec(vec![1, 2]);
        let list2 = List::from_vec(vec![3, 4]);
        let appended = list1.append(&list2);

        assert_eq!(appended.len(), 4);
        assert_eq!(appended.to_vec(), vec![1, 2, 3, 4]);

        // Original lists unchanged
        assert_eq!(list1.to_vec(), vec![1, 2]);
        assert_eq!(list2.to_vec(), vec![3, 4]);
    }

    #[test]
    fn test_reverse() {
        let list = List::from_vec(vec![1, 2, 3]);
        let reversed = list.reverse();

        assert_eq!(reversed.to_vec(), vec![3, 2, 1]);
        // Original list unchanged
        assert_eq!(list.to_vec(), vec![1, 2, 3]);
    }

    #[test]
    fn test_map() {
        let list = List::from_vec(vec![1, 2, 3]);
        let doubled = list.map(|x| x * 2);

        assert_eq!(doubled.to_vec(), vec![2, 4, 6]);
        // Original list unchanged
        assert_eq!(list.to_vec(), vec![1, 2, 3]);
    }

    #[test]
    fn test_map_different_types() {
        let list = List::from_vec(vec![1, 2, 3]);
        let strings = list.map(|x| x.to_string());

        assert_eq!(
            strings.to_vec(),
            vec!["1".to_string(), "2".to_string(), "3".to_string()]
        );
    }

    #[test]
    fn test_filter() {
        let list = List::from_vec(vec![1, 2, 3, 4, 5]);
        let evens = list.filter(|x| *x % 2 == 0);

        assert_eq!(evens.to_vec(), vec![2, 4]);
        // Original list unchanged
        assert_eq!(list.to_vec(), vec![1, 2, 3, 4, 5]);
    }

    #[test]
    fn test_fold_operations() {
        let list = List::from_vec(vec![1, 2, 3, 4]);

        let sum_left = list.fold_left(0, |acc, x| acc + x);
        assert_eq!(sum_left, 10);

        let sum_right = list.fold_right(0, |x, acc| x + acc);
        assert_eq!(sum_right, 10);

        // Test that order matters for non-commutative operations
        let concat_left = list.fold_left(String::new(), |acc, x| format!("{}{}", acc, x));
        assert_eq!(concat_left, "1234");

        let concat_right = list.fold_right(String::new(), |x, acc| format!("{}{}", x, acc));
        assert_eq!(concat_right, "1234");
    }

    #[test]
    fn test_take_and_drop() {
        let list = List::from_vec(vec![1, 2, 3, 4, 5]);

        let taken = list.take(3);
        assert_eq!(taken.to_vec(), vec![1, 2, 3]);

        let dropped = list.drop_front(2);
        assert_eq!(dropped.to_vec(), vec![3, 4, 5]);

        // Edge cases
        let take_all = list.take(10);
        assert_eq!(take_all.to_vec(), vec![1, 2, 3, 4, 5]);

        let take_none = list.take(0);
        assert!(take_none.is_empty());

        let drop_all = list.drop_front(10);
        assert!(drop_all.is_empty());

        let drop_none = list.drop_front(0);
        assert_eq!(drop_none.to_vec(), vec![1, 2, 3, 4, 5]);
    }

    #[test]
    fn test_get_by_index() {
        let list = List::from_vec(vec![10, 20, 30]);

        assert_eq!(list.get(0), Some(&10));
        assert_eq!(list.get(1), Some(&20));
        assert_eq!(list.get(2), Some(&30));
        assert_eq!(list.get(3), None);
        assert_eq!(list.get(100), None);
    }

    #[test]
    fn test_iterator() {
        let list = List::from_vec(vec![1, 2, 3]);

        let collected: Vec<&i32> = list.iter().collect();
        assert_eq!(collected, vec![&1, &2, &3]);

        // Test iterator with functional operations
        let sum: i32 = list.iter().sum();
        assert_eq!(sum, 6);

        let doubled: Vec<i32> = list.iter().map(|x| x * 2).collect();
        assert_eq!(doubled, vec![2, 4, 6]);
    }

    #[test]
    fn test_display() {
        let list = List::from_vec(vec![1, 2, 3]);
        let display_str = format!("{}", list);
        assert_eq!(display_str, "[1, 2, 3]");

        let empty: List<i32> = List::new();
        assert_eq!(format!("{}", empty), "[]");

        let singleton = List::singleton(42);
        assert_eq!(format!("{}", singleton), "[42]");
    }

    #[test]
    fn test_vec_roundtrip() {
        let original_vec = vec![1, 2, 3, 4, 5];
        let list = List::from_vec(original_vec.clone());
        let recovered_vec = list.to_vec();

        assert_eq!(original_vec, recovered_vec);
    }

    #[test]
    fn test_immutability() {
        let list1 = List::from_vec(vec![1, 2, 3]);
        let list2 = list1.push_front(0);
        let list3 = list1.append(&List::singleton(4));

        // All lists should be independent
        assert_eq!(list1.to_vec(), vec![1, 2, 3]);
        assert_eq!(list2.to_vec(), vec![0, 1, 2, 3]);
        assert_eq!(list3.to_vec(), vec![1, 2, 3, 4]);
    }

    #[test]
    fn test_large_list_performance() {
        // Test that operations work with larger lists
        let large_vec: Vec<i32> = (0..1000).collect();
        let large_list = List::from_vec(large_vec.clone());

        // Head and tail should be O(1)
        assert_eq!(large_list.head(), Some(&0));
        assert_eq!(large_list.tail().unwrap().head(), Some(&1));

        // Length should work (though O(n))
        assert_eq!(large_list.len(), 1000);

        // Take should work efficiently
        let first_10 = large_list.take(10);
        assert_eq!(first_10.to_vec(), (0..10).collect::<Vec<i32>>());
    }

    #[test]
    fn test_recursive_operations() {
        // Test that recursive operations don't overflow stack for reasonable sizes
        let list = List::from_vec((0..100).collect::<Vec<i32>>());

        let doubled = list.map(|x| x * 2);
        assert_eq!(doubled.head(), Some(&0));
        assert_eq!(doubled.tail().unwrap().head(), Some(&2));

        let sum = list.fold_left(0, |acc, x| acc + x);
        assert_eq!(sum, 4950); // Sum of 0..99

        let reversed = list.reverse();
        assert_eq!(reversed.head(), Some(&99));
        assert_eq!(reversed.tail().unwrap().head(), Some(&98));
    }
}
