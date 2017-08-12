;;; `cfs--custom-set-fontsnames' 列表有3个子列表，第1个为英文字体列表，第2个为中文字体列表，
;;; 第3个列表中的字体用于显示不常用汉字，每一个字体列表中，*第一个* *有效并可用* 的字体将被使用。
;;; 将光标移动到上述列表中，按 `C-c C-c' 可以测试字体显示效果。另外，用户可以通过命令
;;; `cfs-insert-fontname’ 来选择一个 *可用* 字体，然后在当前光标处插入其字体名称。
(setq cfs--custom-set-fontnames
      '(
        ("Inconsolata" "Ubuntu Mono" "Times New Roman" "Consolas" "Lucida Sans Typewriter" "Liberation Mono" "Lucida Console" "Andale Mono" "Courier" "DejaVu Sans Mono" "Courier New" "Free Mono" "Monaco" "Droid Sans Mono" "PragmataPro" "MonacoB" "MonacoB2" "MonacoBSemi" "Droid Sans Mono Pro" "Source Code Pro" "Envy Code R" "monoOne" "Lucida Typewriter" "Panic Sans" "Hack" "Bitstream Vera Sans Mono" "HyperFont" "PT Mono" "Ti92Pluspc" "Excalibur Monospace" "Menlof" "Cousine" "Fira Mono" "Lekton" "M+ 1mn" "BPmono" "Anonymous Pro" "ProFont" "ProFontWindows" "Latin Modern Mono" "Code 2002" "ProggyCleanTT" "ProggyTinyTT")
        ("STKaiti" "宋体" "STXihei" "STSong" "STZhongsong" "STFangsong" "FZShuTi" "FZYaoti" "STCaiyun" "STHupo" "STLiti" "STXingkai" "STXinwei" "新宋体" "YouYuan" "LiSu" "KaiTi" "华文行楷" "华文细黑" "华文新魏" "华文彩云" "华文中宋" "华文仿宋" "方正姚体" "隶书" "幼圆" "FangSong" "NSimSun" "SimSun" "SimHei" "黑体" "文泉驿等宽微米黑" "微软雅黑" "Times New Roman" "Noto Sans S Chinese Regular" "Microsoft Yahei" "Microsoft_Yahei" "Ubuntu Mono" "文泉驿等宽正黑" "Hiragino Sans GB" "文泉驿正黑" "文泉驿点阵正黑" "FangSong_GB2312" "KaiTi_GB2312" "楷体_GB2312" "仿宋_GB2312" "方正舒体" "方正粗圆_GBK")
        ("MingLiU_HKSCS-ExtB" "SimSun-ExtB" "MingLiU-ExtB" "PMingLiU-ExtB" "HanaMinB")
        ))

;;; `cfs--custom-set-fontsizes' 中，所有元素的结构都类似：(英文字号 中文字号 EXT-B字体字号)
;;; 将光标移动到各个数字上，按 C-c C-c 查看光标处字号的对齐效果。
;;; 按 C-<up> 增大光标处字号，按 C-<down> 减小光标处字号。
(setq cfs--custom-set-fontsizes
      '(
        (9    9.0  9.0 )
        (10   10.5 10.5)
        (11.5 12.0 12.0)
        (12.5 13.5 13.5)
        (14   15.0 15.0)
        (15   15.0 15.0)
        (16   16.5 16.5)
        (18   18.0 18.0)
        (20   24.0 24.0)
        (22   25.5 25.5)
        (24   28.5 28.5)
        (26   31.5 31.5)
        (28   33.0 33.0)
        (30   36.0 36.0)
        (32   39.0 39.0)
        ))
