import assert from "node:assert";

type LinkedList<A> = null | LinkedListNode<A>;
type LinkedListNode<A> = {
  item: A;
  next: LinkedList<A>;
};

export function fromArray<A>(array: Array<A>): LinkedList<A> {
  let next: LinkedList<A> = null;
  for (let i = array.length - 1; i >= 0; i--) {
    next = { item: array[i], next: next };
  }
  return next;
}

export function toArray<A>(list: LinkedList<A>): Array<A> {
  let r: Array<A> = [];
  while (list) {
    r.push(list.item);
    list = list.next;
  }
  return r;
}

export function reverse<A>(list: LinkedList<A>): LinkedList<A> {
  let next: LinkedList<A> = null;
  while (list) {
    next = { item: list.item, next: next };
    list = list.next;
  }
  return next;
}

export function reverseInPlace<A>(
  list: LinkedList<A>,
  next: LinkedList<A> = null
): LinkedList<A> {
  if (!list) return next;

  let newHead = reverseInPlace(list.next, list);
  list.next = next;
  return newHead;
}

function test() {
  assert.deepEqual([], toArray(fromArray([])));
  assert.deepEqual([1, 2, 3], toArray(fromArray([1, 2, 3])));

  assert.deepEqual([], toArray(reverse(fromArray([]))));
  assert.deepEqual([3, 2, 1], toArray(reverse(fromArray([1, 2, 3]))));

  assert.deepEqual([], toArray(reverseInPlace(fromArray([]))));
  assert.deepEqual([3, 2, 1], toArray(reverseInPlace(fromArray([1, 2, 3]))));
}
test();
