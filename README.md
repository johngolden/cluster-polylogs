# Cluster Polylog Github Usage Tips

Start by cloning the repo: `git clone https://github.com/johngolden/cluster-polylogs`.

For now we'll avoid any fancy branching/pulling/merging stuff and just treat this is as a more difficult to use Dropbox ;) 

`john/` and `andrew/` are for our respective mathematica codes that we'd like the other to have access to, and `notes/` is where we can each store any research notes we have. Eventually there will also be a `paper/` folder. We won't edit things in each other's code folders, but are free to copy code from one another. 

To add a new file to your folder, navigate there in Terminal and then run

	git add hello-world.txt
	git commit -m "(brief explanation of what hello-world.txt does)"
	git push

Run the same commands to upload any changes to a file, e.g. 
	
	git add hello-world.txt
	git commit -m "(brief explanation of the change made to hello-world.txt)"
	git push

Run `git pull` periodically to download what the other person has uploaded.
