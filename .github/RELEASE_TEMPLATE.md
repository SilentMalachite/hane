# Hane vX.Y.Z (YYYY-MM-DD)

## ハイライト
- 主要な改善点や新機能を1–3行で。

## 変更点
### Added
- 

### Changed
- 

### Fixed
- 

### Removed
- 

## 互換性/破壊的変更
- 破壊的変更があれば内容と影響、移行手順を明記。

## 移行ガイド
- バージョン間の移行手順があれば記載。

## ダウンロード
- リリース資産にビルド済みバイナリを添付した場合、その一覧と対応 OS/Arch を記載。

## チェックサム（任意）
```
sha256sum hane-<os>-<arch>.tar.gz
```

## 対応環境
- GHC: 9.6.x
- OS: macOS/Linux（Windows は実験的）

## インストール/実行

### バイナリから（推奨）
1. Assets からお使いのOSに対応するファイルをダウンロード
2. 解凍して実行可能ファイルをPATHの通った場所に配置
3. `hane` コマンドで起動

### ソースから
```bash
# 基本インストール
git clone https://github.com/SilentMalachite/hane.git
cd hane
cabal build
cabal run hane

# ICU サポート付き（オプション）
# ICUライブラリが必要（README参照）
cabal build -f +icu
cabal run -f +icu hane
```

## 比較リンク
- 変更比較: https://github.com/SilentMalachite/hane/compare/vA.B.C...vX.Y.Z

## 謝辞
- コントリビュータの皆様に感謝します！
