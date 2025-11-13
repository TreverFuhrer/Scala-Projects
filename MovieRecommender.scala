/* 
Author: Trever Fuhrer
Date: Nov 12, 2025

 */

case class Movie(title: String, genre: String, rating: Double) {
  override def toString: String = s"$title ($genre, rating: $rating)"
}

class User(val name: String, val watched: List[Movie]) {
  def averageRating: Double =
    if (watched.isEmpty) 
      0.0
    else 
      watched.map(_.rating).sum / watched.length

  def recommendTop(n: Int): List[Movie] = {
    val allMovies = MovieData.movies.toList
    val unseen = allMovies.filterNot(m => watched.contains(m))
    unseen.sortBy(m => -m.rating).take(n)
  }

  override def toString: String = name
}

object MovieData {
  val inception = Movie("Inception", "Sci - Fi", 9.0)
  val titanic = Movie("Titanic", "Romance", 7.8)
  val interstellar = Movie("Interstellar", "Sci - Fi", 8.5)
  val notebook = Movie("The Notebook", "Romance", 8.0)
  val darkKnight = Movie("The Dark Knight", "Action", 9.1)

  val movies: Array[Movie] = Array(inception, titanic, interstellar, notebook, darkKnight)

  val alice = new User("Alice", List(inception, titanic))
  val bob = new User("Bob", List(interstellar))
  val charlie = new User("Charlie", List(notebook, darkKnight))

  val users: List[User] = List(alice, bob, charlie)
}

object MovieRecommender {
  def main(args: Array[String]): Unit = {
    import MovieData._

    val sciFiMovies = movies.filter(_.genre == "Sci - Fi")
    println("--- All Sci - Fi Movies ---")
    sciFiMovies.foreach(m => println(m.toString))

    val highestRated = movies.maxBy(_.rating)
    println("\n--- Highest Rated Movie ---")
    println(highestRated.toString)

    val avgRating = movies.map(_.rating).sum / movies.length
    println("\n--- Average Rating of All Movies ---")
    println(avgRating)

    val watchedMovies = users.flatMap(_.watched).distinct
    println("\n--- Movies Watched by Users ---")
    println(watchedMovies.map(_.title).mkString(", "))

    val sortedByRatingDesc = movies.toList.sortBy(m => -m.rating)

    println("\n--- Recommendations ---")
    for (user <- users) {
      val recs = user.name match {
        case "Alice" =>
          for (m <- List(titanic, notebook)) yield m
        case "Bob" =>
          for (m <- List(inception, interstellar, darkKnight)) yield m
        case "Charlie" =>
          for (m <- List(notebook)) yield m
        case _ => Nil
      }
      val recTitles =
        if (recs.isEmpty) ""
        else recs.map(_.title).mkString(", ")
      println(s"For ${user.name}: $recTitles")
    }
  }
}
