package yumemi.serverside

import com.opencsv.{CSVParserBuilder, CSVReaderBuilder, CSVReader}
import java.io.FileReader
import scala.annotation.tailrec

object Main extends Ranking {
  def main(args: Array[String]): Unit = {
    // このコマンドは引数を１つ(ゲームプレイログが記録されたファイル名)だけ持つ
    val fileName = if (args.length == 1) {
      args(0)
    } else {
      throw new IllegalArgumentException
    }

    // CSVファイルの読み込みにはOpenCSVを使用する
    val reader = new CSVReaderBuilder(new FileReader(fileName))
      .withCSVParser(new CSVParserBuilder().withSeparator(',').build())
      .withSkipLines(1)
      .build()

    try {
      // 2つの関数(processFile, extractTop10)を合成した関数に処理対象のファイルを渡して呼び出している
      val results = (processFile _ andThen extractTop10)(reader)

      // ヘッダーの出力
      println("rank,player_id,mean_score")

      results.foreach(ranking => {
        val (rank, scores) = ranking // Tuple2の要素をパターンマッチで抽出
        println(s"$rank,${scores.playerId},${scores.meanScore}")
      })
    } catch {
      case e =>
        e.printStackTrace()
    }

    reader.close()
  }
}

trait Ranking {
  val MAX_RANKING = 10
  type T = Map[String, Scores]

  def processFile(reader: CSVReader): T = {
    @tailrec // 末尾再帰
    def totalScore(z: T, op: (T, CSVData) => T): T = {
      val line = reader.readNext()
      if (line == null) {
        z
      } else {
        totalScore(op(z, CSVData(line(0), line(1), line(2).toInt)), op)
      }
    }

    // Playerごとにスコアを合計していく
    totalScore(Map[String, Scores](), (playingRecord, data) => {
      val scores = playingRecord.get(data.playerId) match {
        case Some(scores) => scores.addScore(data.score)
        case None => Scores(data.playerId, 1, data.score)
      }
      playingRecord + (scores.playerId -> scores)
    })
  }

  def extractTop10(playingRecords: Map[String, Scores]): List[(Int, Scores)] = {
    // 平均点が高い順番にソートする
    val sortedPlayingRecords = playingRecords.values.toList.sortWith((s1, s2) => s1.meanScore > s2.meanScore)

    @tailrec // 末尾再帰
    def _extractTop10(rank: Int, top10: List[(Int, Scores)], others: List[Scores]): List[(Int, Scores)] = {
      val lastMeanScore = if (top10.isEmpty) Int.MaxValue else top10.last._2.meanScore
      others match {
        case head :: tail if top10.size <= MAX_RANKING || head.meanScore == lastMeanScore =>
          val nextRank = if (head.meanScore < lastMeanScore) top10.size + 1 else rank
          _extractTop10(nextRank, top10 :+ (nextRank, head), tail)
        case Nil => top10
      }
    }

    _extractTop10(0, List(), sortedPlayingRecords)
  }
}

case class CSVData(createTimestamp: String, playerId: String, score: Int)

case class Scores(playerId: String, numOfPlaying: Int, total: Double) {
  // 1度だけ計算するためにlazyをつけている。1度呼び出したら再計算されないので注意。
  lazy val meanScore: Int = Math.round(total / numOfPlaying).toInt

  // Scoresクラスは、immutable。新しい値を設定したScoresを返す。
  def addScore(score: Int): Scores = {
    this.copy(
      numOfPlaying = this.numOfPlaying + 1,
      total = this.total + score)
  }
}
