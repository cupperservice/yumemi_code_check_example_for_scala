package yumemi.serverside

import java.io.FileReader
import java.io.File
import com.opencsv.bean.CsvToBeanBuilder
import com.opencsv.bean.CsvBindByName
import scala.annotation.tailrec
import scala.collection.JavaConverters._

object Main extends Ranking {
  def main(args: Array[String]): Unit = {
    // このコマンドは引数を１つ(ゲームプレイログが記録されたファイル名)だけ持つ
    val fileName = if (args.length == 1) {
      args(0)
    } else {
      throw new IllegalArgumentException
    }

    // ヘッダーの出力
    println("rank,player_id,mean_score")

    // 2つの関数(processFile, extractTop10)を合成した関数に処理対象のファイルを渡して呼び出している
    (processFile _ andThen extractTop10)(new File(fileName)).foreach(ranking => {
      val (rank, scores) = ranking // Tuple2の要素をパターンマッチで抽出
      println(s"${rank},${scores.playerId},${scores.meanScore}")
    })
  }
}

trait Ranking {
  def processFile(csvFile: File): Map[String, Scores] = {
    // CSVファイルの読み込みにはOpenCSVを使用する
    val reader = new CsvToBeanBuilder[CSVData](new FileReader(csvFile)).withType(classOf[CSVData]).build()

    // Playerごとにスコアを合計していく
    reader.asScala.foldLeft(Map[String, Scores]()) { (playingRecord, data) =>
      val scores = playingRecord.get(data.player_id) match {
        case Some(scores) => scores.addScore(data.score)
        case None => Scores(data.player_id, 1, data.score)
      }
      playingRecord + (scores.playerId -> scores)
    }
  }

  def extractTop10(playingRecords: Map[String, Scores]): List[(Int, Scores)] = {
    // 平均点が高い順番にソートする
    val sortedPlayingRecords = playingRecords.values.toList.sortWith((s1, s2) => s1.meanScore > s2.meanScore)

    @tailrec // 末尾再帰
    def _extractTop10(rank: Int, top10: List[(Int, Scores)], others: List[Scores]): List[(Int, Scores)] = {
      val lastMeanScore = if (top10.isEmpty) Int.MaxValue else top10.last._2.meanScore
      others match {
        case head :: tail if (top10.size <= 10 || head.meanScore == lastMeanScore) =>
          val nextRank = if (head.meanScore < lastMeanScore) top10.size + 1 else rank
          _extractTop10(nextRank, top10 :+ (nextRank, head), tail)
        case Nil => top10
      }
    }
    _extractTop10(0, List(), sortedPlayingRecords)
  }
}

class CSVData(
               @CsvBindByName(column = "create_timestamp") val create_timestamp: String,
               @CsvBindByName(column = "player_id") val player_id: String,
               @CsvBindByName(column = "score") val score: Int) {

  // OpenCSVで使うためにデフォルトコンストラクタが必要
  def this() = this("", "", 0)
}

case class Scores(val playerId: String, val numOfPlaying: Int, total: Double) {
  // 1度だけ計算するためにlazyをつけている。1度呼び出したら再計算されないので注意。
  lazy val meanScore = Math.round(total / numOfPlaying).toInt

  // Scoresクラスは、immutable。新しい値を設定したScoresを返す。
  def addScore(score: Int): Scores = {
    this.copy(
      numOfPlaying = this.numOfPlaying + 1,
      total = this.total + score)
  }
}
