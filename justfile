version:="release_v$(eval cat version)"

bump:
	nvim version
	sed -i "s/Version: \`[0-9]\.[0-9]\.[0-9]\`/Version: \`$(cat version)\`/g" README.md

release:
	tar cf {{version}}.tar ./data.csv ./README.md
	ouch compress ./data.csv ./README.md {{version}}.zip
