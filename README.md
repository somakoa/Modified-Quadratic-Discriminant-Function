MQDFのライブラリ作ってみました

auther:	Somada


Ver.1.0


--- 使い方 ---

学習　（辞書作成）
./mqdf-train [option] training_data_dir dictionary_dir
[option]
-p 変数変換のパラメータ (  ~ 1 ) DEFAULT 1
   			入力データに負の値がある場合はエラー
			

認識
./mqdf-predict [option] dictionary_dir predict_datafile
[option]
-a alphaの設定 (0 ~ 1) DEFAULT 0.1
-k 使用する固有ベクトルの数 ( 0 ~ 入力データの次元数 )
-o 出力ファイル



example:

bin/mqdf-train test/iris/train test/iris/dictionary
bin/mqdf-predict -o result.txt test/iris/dictionary test/iris/test/iris_all.libsvm
