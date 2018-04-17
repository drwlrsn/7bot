package domain

import github4s.free.domain.Label

class Review(
     url: String,
     title: String,
     approved: List[Int] = List.empty,
     changes: List[Int] = List.empty,
     updatedAt: Option[String],
     merged_at: Option[String],
     created_at: String,
     closed_at: Option[String],
     labels: List[Label] = List.empty);
