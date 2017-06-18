# これなに?

* http://www.cl.ecei.tohoku.ac.jp/nlp100/
* 上記をHaskellで解いてみている

# TODO

- 第1章: 準備運動
  - [x] 00. 文字列の逆順
  - [x] 01. 「パタトクカシーー」
  - [x] 02. 「パトカー」＋「タクシー」＝「パタトクカシーー」
  - [x] 03. 円周率
  - [x] 04. 元素記号
  - [x] 05. n-gram
  - [x] 06. 集合
  - [x] 07. テンプレートによる文生成
  - [x] 08. 暗号文
  - [x] 09. Typoglycemia
- 第2章: UNIXコマンドの基礎
  - [x] 10. 行数のカウント
  - [x] 11. タブをスペースに置換
  - [x] 12. 1列目をcol1.txtに，2列目をcol2.txtに保存
  - [x] 13. col1.txtとcol2.txtをマージ
  - [x] 14. 先頭からN行を出力
  - [x] 15. 末尾のN行を出力
  - [x] 16. ファイルをN分割する
  - [x] 17. １列目の文字列の異なり
  - [x] 18. 各行を3コラム目の数値の降順にソート
  - [x] 19. 各行の1コラム目の文字列の出現頻度を求め，出現頻度の高い順に並べる
- 第3章: 正規表現
  - [x] 20. JSONデータの読み込み
  - [x] 21. カテゴリ名を含む行を抽出
  - [x] 22. カテゴリ名の抽出
  - [x] 23. セクション構造
  - [x] 24. ファイル参照の抽出
  - [x] 25. テンプレートの抽出
  - [x] 26. 強調マークアップの除去
  - [x] 27. 内部リンクの除去
  - [x] 28. MediaWikiマークアップの除去
  - [ ] 29. 国旗画像のURLを取得する
    - 面倒だったから未実施
- 第4章: 形態素解析
  - [x] 30. 形態素解析結果の読み込み
  - [x] 31. 動詞
  - [x] 32. 動詞の原形
  - [x] 33. サ変名詞
  - [x] 34. 「AのB」
  - [x] 35. 名詞の連接
  - [x] 36. 単語の出現頻度
  - [x] 37. 頻度上位10語
  - [x] 38. ヒストグラム
  - [x] 39. Zipfの法則
- 第5章: 係り受け解析
  - [x] 40. 係り受け解析結果の読み込み（形態素）
  - [x] 41. 係り受け解析結果の読み込み（文節・係り受け）
  - [x] 42. 係り元と係り先の文節の表示
  - [x] 43. 名詞を含む文節が動詞を含む文節に係るものを抽出
  - [x] 44. 係り受け木の可視化
  - [x] 45. 動詞の格パターンの抽出
  - [x] 46. 動詞の格フレーム情報の抽出
  - [x] 47. 機能動詞構文のマイニング
  - [x] 48. 名詞から根へのパスの抽出
  - [ ] 49. 名詞間の係り受けパスの抽出
- 第6章: 英語テキストの処理
  - [ ] 50. 文区切り
  - [ ] 51. 単語の切り出し
  - [ ] 52. ステミング
  - [ ] 53. Tokenization
  - [ ] 54. 品詞タグ付け
  - [ ] 55. 固有表現抽出
  - [ ] 56. 共参照解析
  - [ ] 57. 係り受け解析
  - [ ] 58. タプルの抽出
  - [ ] 59. S式の解析
- 第7章: データベース
  - [ ] 60. KVSの構築
  - [ ] 61. KVSの検索
  - [ ] 62. KVS内の反復処理
  - [ ] 63. オブジェクトを値に格納したKVS
  - [ ] 64. MongoDBの構築
  - [ ] 65. MongoDBの検索
  - [ ] 66. 検索件数の取得
  - [ ] 67. 複数のドキュメントの取得
  - [ ] 68. ソート
  - [ ] 69. Webアプリケーションの作成
- 第8章: 機械学習
  - [ ] 70. データの入手・整形
  - [ ] 71. ストップワード
  - [ ] 72. 素性抽出
  - [ ] 73. 学習
  - [ ] 74. 予測
  - [ ] 75. 素性の重み
  - [ ] 76. ラベル付け
  - [ ] 77. 正解率の計測
  - [ ] 78. 5分割交差検定
  - [ ] 79. 適合率-再現率グラフの描画
- 第9章: ベクトル空間法 (I)
  - [ ] 80. コーパスの整形
  - [ ] 81. 複合語からなる国名への対処
  - [ ] 82. 文脈の抽出
  - [ ] 83. 単語／文脈の頻度の計測
  - [ ] 84. 単語文脈行列の作成
  - [ ] 85. 主成分分析による次元圧縮
  - [ ] 86. 単語ベクトルの表示
  - [ ] 87. 単語の類似度
  - [ ] 88. 類似度の高い単語10件
  - [ ] 89. 加法構成性によるアナロジー
- 第10章: ベクトル空間法 (II)
  - [ ] 90. word2vecによる学習
  - [ ] 91. アナロジーデータの準備
  - [ ] 92. アナロジーデータへの適用
  - [ ] 93. アナロジータスクの正解率の計算
  - [ ] 94. WordSimilarity-353での類似度計算
  - [ ] 95. WordSimilarity-353での評価
  - [ ] 96. 国名に関するベクトルの抽出
  - [ ] 97. k-meansクラスタリング
  - [ ] 98. Ward法によるクラスタリング
  - [ ] 99. t-SNEによる可視化
