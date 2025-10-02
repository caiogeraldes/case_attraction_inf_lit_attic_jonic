version:="$(eval cat version)"

default:
	just --list

bump:
	nvim version
	sed -i "s/Version: \`[0-9]\.[0-9]\.[0-9]\`/Version: \`$(cat version)\`/g" README.md

release:
	tar cf release_v{{version}}.tar ./data.csv ./README.md ./scripts/
	ouch compress ./data.csv ./README.md ./scripts/ release_v{{version}}.zip
	gh release create v{{version}} 
	gh release upload v{{version}} release_v{{version}}.tar release_v{{version}}.zip
