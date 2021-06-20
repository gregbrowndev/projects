package fr_ddd.chapter3.lens

case class Lens[O, V] (
  get: O => V,
  set: (O, V) => O
)
