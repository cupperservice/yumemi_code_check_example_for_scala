package yumemi.serverside

import com.opencsv.{CSVParserBuilder, CSVReaderBuilder}
import org.scalatest.AsyncFlatSpec

import java.io.{File, FileReader}

class RankingTest extends AsyncFlatSpec with Ranking {
  def test(file: File): List[(Int, Scores)] = {
    val reader = new CSVReaderBuilder(new FileReader(file))
      .withCSVParser(new CSVParserBuilder().withSeparator(',').build())
      .withSkipLines(1)
      .build()

    (processFile _ andThen extractTop10)(reader)
  }

  "ranking" should "empty" in {
    val file = new File(getClass.getResource("/empty.csv").getFile)
    assert(test(file).isEmpty)
  }

  "ranking" should "just 10" in {
    val file = new File(getClass.getResource("/just10.csv").getFile)
    assert(test(file) == List(
      (1,Scores("player0010",2,2000.0)),
      (2,Scores("player0009",2,1800.0)),
      (3,Scores("player0008",2,1600.0)),
      (4,Scores("player0007",2,1400.0)),
      (5,Scores("player0006",2,1200.0)),
      (6,Scores("player0005",2,1000.0)),
      (7,Scores("player0004",2,800.0)),
      (8,Scores("player0003",2,600.0)),
      (9,Scores("player0002",2,400.0)),
      (10,Scores("player0001",2,200.0)),
    ))
  }

  "ranking" should "round" in {
    val file = new File(getClass.getResource("/round.csv").getFile)
    assert(test(file) == List(
      (1,Scores("player0001",2,4.0)),
      (1,Scores("player0002",2,3.0)),
    ))
  }

  "" should "" in {
    val file = new File(getClass.getResource("/data1.csv").getFile)
    assert(test(file) == List(
      (1,Scores("player0001",1,12345.0)),
      (2,Scores("player0002",1,10000.0)),
      (3,Scores("player0041",1,300.0)),
      (3,Scores("player0042",1,300.0)),
      (5,Scores("player0031",1,200.0)),
      (6,Scores("player0011",2,245.0)),
      (7,Scores("player0024",1,100.0)),
      (7,Scores("player0025",1,100.0)),
      (7,Scores("player0022",1,100.0)),
      (7,Scores("player0021",1,100.0)),
      (7,Scores("player0023",1,100.0)),
    ))
  }
}
