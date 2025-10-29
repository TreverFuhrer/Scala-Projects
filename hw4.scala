/* 
Author: Trever Fuhrer
Date: Oct 28, 2025
• What was the most challenging part of this assignment?
    
• How did you use higher-order functions effectively?

• What did you learn about immutability and side effects?

 */

object Main {
    def main(args: Array[String]): Unit = {

        /*   Part 1  */
        val steps : Array [Int] = Array(9500, 10300, 8700, 4000, 12000, 3000, 7600, 9800, 8500, 6600, 10200, 7400, 5600, 8900)

        /*   Part 3  */
        println("Daily steps: " + steps.mkString(", "))
        println("Categories: " + classifyAll(steps).mkString(", "))
        val (minSteps, maxSteps, avgSteps) = summarize(steps)
        println(f"Summary: Min: $minSteps, Max: $maxSteps, Avg: $avgSteps%.2f")
        println("Days meeting goal: " + countDays(steps, (s: Int) => s >= 10000))
        println("Sedentary days: " + countDays(steps, (s: Int) => s < 5000))
        println("Trend: " + compareWeeks(steps))

    }
}

/*   Part 2  */
// 2.1
def classifyAll(data: Array[Int]): Array[String] = {
    data.map(classifySteps)
}

def classifySteps(daySteps: Int): String = {
    if (daySteps < 5000) {
        "Sedentary"
    } 
    else if (daySteps < 9999) {
        "Active"
    }
    else {
        "Highly Active"
    }
}

// 2.2
def summarize(data : Array [Int]) : (Int, Int, Double) = {
    (data.min, data.max, data.sum.toDouble / data.length)
}

// 2.3
def countDays(data : Array [Int], condition : Int => Boolean ) : Int = {
    data.count(condition)
}

// 2.4
def compareWeeks(data : Array [Int]) : String = {
    val (week1, week2) = data.splitAt(7)
    val avg1 = week1.sum.toDouble / week1.length
    val avg2 = week2.sum.toDouble / week2.length

    if ((avg1 - avg2).abs <= 100) {
        "No change"
    } 
    else if (avg2 > avg1) {
        "Improved"
    } 
    else {
        "Declined"
    }
}