use std::fmt::{Debug, Formatter};

pub type NodeId = usize;

pub struct Node<T> {
    pub index: NodeId,
    pub value: T,
    parent: Option<usize>,
}

pub struct Tree<T> {
    nodes: Vec<Node<T>>,
}

impl<T> Debug for Tree<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Tree {{ node_count: {} }}", self.nodes.len())
    }
}

impl<T> Default for Tree<T> {
    fn default() -> Self {
        Self { nodes: vec![] }
    }
}

impl<T> Tree<T> {
    pub fn new() -> Tree<T> {
        Tree { nodes: vec![] }
    }

    pub fn add(&mut self, value: T, parent: Option<NodeId>) -> NodeId {
        let index = self.nodes.len();

        self.nodes.push(Node {
            index,
            value,
            parent,
        });

        index
    }

    #[inline]
    pub fn get(&self, id: NodeId) -> Option<&Node<T>> {
        self.nodes.get(id)
    }

    #[inline]
    pub fn get_mut(&mut self, id: NodeId) -> Option<&mut Node<T>> {
        self.nodes.get_mut(id)
    }

    pub fn cursor(&self) -> Cursor<'_, T> {
        Cursor::new(&self.nodes)
    }
}

pub struct Cursor<'t, T> {
    nodes: &'t [Node<T>],
    pos: NodeId,
}

impl<'t, T> Default for Cursor<'t, T> {
    fn default() -> Self {
        Cursor { nodes: &[], pos: 0 }
    }
}

impl<'t, T> Clone for Cursor<'t, T> {
    fn clone(&self) -> Self {
        Self {
            nodes: self.nodes,
            pos: self.pos,
        }
    }
}

impl<'t, T> Cursor<'t, T> {
    pub fn new(tree: &'t [Node<T>]) -> Cursor<'t, T> {
        Cursor {
            nodes: tree,
            pos: 0,
        }
    }

    pub fn move_to(&mut self, idx: NodeId) {
        self.pos = idx;
    }

    pub fn move_to_end(&mut self) {
        self.pos = self.nodes.len();
    }

    pub fn move_to_parent(&mut self) -> bool {
        if let Some(pos) = self.node().and_then(|n| n.parent) {
            self.pos = pos;
            return true;
        }

        false
    }

    pub fn node(&self) -> Option<&'t Node<T>> {
        self.nodes.get(self.pos)
    }

    pub fn next_sibling(&mut self) -> bool {
        let parent = self.node().and_then(|n| n.parent);

        for i in self.pos + 1..self.nodes.len() {
            if self.nodes.get(i).and_then(|n| n.parent) == parent {
                self.move_to(i);
                return true;
            }
        }

        false
    }

    pub fn next_child(&mut self) -> bool {
        let root = self.node().map(|n| n.index);

        for i in self.pos + 1..self.nodes.len() {
            if self.nodes.get(i).and_then(|n| n.parent) == root {
                self.move_to(i);
                return true;
            }
        }

        false
    }
}

impl<'t, T> Iterator for Cursor<'t, T> {
    type Item = &'t Node<T>;

    fn next(&mut self) -> Option<Self::Item> {
        let node = self.node();

        if self.next_child() {
            return node;
        }

        if self.next_sibling() {
            return node;
        }

        while self.move_to_parent() {
            if self.next_sibling() {
                return node;
            }
        }

        self.move_to_end();

        node
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn get_tree() -> Tree<&'static str> {
        let mut tree = Tree::new();

        let root1 = tree.add("root1", None);
        let root2 = tree.add("root2", None);
        let root3 = tree.add("root3", None);

        tree.add("root3sub1", Some(root3));
        tree.add("root1sub1", Some(root1));
        let root2_sub1 = tree.add("root2sub1", Some(root2));
        tree.add("root1sub2", Some(root1));
        tree.add("root2sub1sub1", Some(root2_sub1));
        tree.add("root2sub2", Some(root2));
        tree.add("root2sub1sub2", Some(root2_sub1));
        tree.add("root4", None);

        tree
    }

    #[test]
    pub fn test_next_sibling() {
        let tree = get_tree();
        let mut cursor = tree.cursor();
        let mut values = vec![];

        loop {
            if let Some(n) = cursor.node() {
                values.push(n.value);
            }

            if !cursor.next_sibling() {
                break;
            }
        }

        assert_eq!(values, vec!["root1", "root2", "root3", "root4",]);
    }

    #[test]
    pub fn test_next_child() {
        let tree = get_tree();
        let mut cursor = tree.cursor();
        let mut values = vec![];

        cursor.move_to(1);

        loop {
            if let Some(n) = cursor.node() {
                values.push(n.value);
            }

            if !cursor.next_child() {
                break;
            }
        }

        assert_eq!(values, vec!["root2", "root2sub1", "root2sub1sub1",]);
    }

    #[test]
    pub fn test_walk() {
        let tree = get_tree();
        let cursor = tree.cursor();
        let mut values = vec![];

        for n in cursor {
            values.push(n.value);
        }

        assert_eq!(
            values,
            vec![
                "root1",
                "root1sub1",
                "root1sub2",
                "root2",
                "root2sub1",
                "root2sub1sub1",
                "root2sub1sub2",
                "root2sub2",
                "root3",
                "root3sub1",
                "root4",
            ]
        );
    }
}
