;;; `cfs--custom-set-fontsnames' 列表有3个子列表，第1个为英文字体列表，第2个为中文字体列表，
;;; 第3个列表中的字体用于显示不常用汉字，每一个字体列表中，*第一个* *有效并可用* 的字体将被使用。
;;; 将光标移动到上述列表中，按 `C-c C-c' 可以测试字体显示效果。另外，用户可以通过命令
;;; `cfs-insert-fontname’ 来选择一个 *可用* 字体，然后在当前光标处插入其字体名称。
(setq cfs--custom-set-fontnames
      '(
        ("Courier 10 Pitch" "Comic Sans MS" "Courier New" "Consolas" "Ubuntu Mono" "Lucida Console" "DejaVu Sans Mono" "Free Mono" "Inconsolata" "Lucida Sans Typewriter" "Liberation Mono" "Andale Mono" "Courier" "Monaco" "Droid Sans Mono" "PragmataPro" "MonacoB" "MonacoB2" "MonacoBSemi" "Droid Sans Mono Pro" "Source Code Pro" "Envy Code R" "monoOne" "Lucida Typewriter" "Panic Sans" "Hack" "Bitstream Vera Sans Mono" "HyperFont" "PT Mono" "Ti92Pluspc" "Excalibur Monospace" "Menlof" "Cousine" "Fira Mono" "Lekton" "M+ 1mn" "BPmono" "Anonymous Pro" "ProFont" "ProFontWindows" "Latin Modern Mono" "Code 2002" "ProggyCleanTT" "ProggyTinyTT")
        ("fangsong ti" "文泉驿点阵正黑" "文泉驿正黑" "文泉驿等宽正黑" "KaiTi" "song ti" "Comic Sans MS" "文泉驿等宽微米黑" "FangSong" "Microsoft Yahei" "黑体" "微软雅黑" "WenQuanYi Micro Hei" "Ubuntu Mono" "WenQuanYi Micro Hei Mono" "STKaiti" "STCaiyun" "FZYaoti" "FZShuTi" "STFangsong" "STZhongsong" "STSong" "STXihei" "隶书" "STHupo" "STLiti" "STXingkai" "STXinwei" "方正姚体" "方正舒体" "华文仿宋" "华文中宋" "华文彩云" "华文新魏" "华文行楷" "华文细黑" "SimHei" "SimSun" "宋体" "新宋体" "LiSu" "幼圆" "YouYuan" "NSimSun" "Noto Sans S Chinese Regular" "Microsoft_Yahei" "Hiragino Sans GB" "FangSong_GB2312" "KaiTi_GB2312" "楷体_GB2312" "仿宋_GB2312" "方正粗圆_GBK")
        ("HanaMinB" "SimSun-ExtB" "MingLiU-ExtB" "PMingLiU-ExtB" "MingLiU_HKSCS-ExtB")
        ))

;;; `cfs--custom-set-fontsizes' 中，所有元素的结构都类似：(英文字号 中文字号 EXT-B字体字号)
;;; 将光标移动到各个数字上，按 C-c C-c 查看光标处字号的对齐效果。
;;; 按 C-<up> 增大光标处字号，按 C-<down> 减小光标处字号。
(setq cfs--custom-set-fontsizes
      '(
        (9    8.5  9.5 )
        (10   12.0 12.0)
        (11.5 13.0 13.5)
        (12.5 15.0 15.0)
        (14   18.5 15.0)
        (15   18.0 16.5)
        (16   19.5 19.5)
        (18   21.0 21.0)
        (20   24.0 24.0)
        (22   25.5 25.5)
        (24   28.5 28.5)
        (26   31.5 31.5)
        (28   33.0 33.0)
        (30   36.0 36.0)
        (32   39.0 39.0)
        ))
