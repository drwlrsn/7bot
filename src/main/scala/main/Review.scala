package main

import github4s.free.domain.Label

case class Review(
  url: String,
  title: String,
  approved: Int = 0,
  changes: Int = 0,
  updatedAt: String,
  created_at: String,
  closed_at: Option[String],
  labels: List[Label] = List.empty

);