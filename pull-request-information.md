This document should provide basic information for making Pull Requests in Moffitt projects.

# What is a Pull Request?

A Pull Request allows other to review your code before it is pushed to the master GitLab branch.  It is opportunity to solicit code review and get feedback on your code.

# What does a Pull Request workflow look like?

In a workflow that utilizes Pull Requests, we must treat the master branch of a repository as sacred. Major changes to code should never be commited to the master without review. There are two main approaches to working with Pull Requests:

1. Work from a branch.
2. Work from a forked repository. It is unlikely that working with fork would be useful for the Moffitt workflow so it is not covered here.

# What are the technical details?

## Working with branches.

Make sure the repository is cloned to your local drive and up to date:

```
git pull origin master
```

Next create a branch, the general formula is:
`git checkout -b <new-branch-name> [<base-branch-name>]`

The base branch defaults to master, so it not necessary to specify `[<base-branch-name>]`. Here is an example creating a new branch called `my-branch`:

```
git checkout -b my-branch
```

This will create a new branch on your machine. You can also create a new branch directly on GitLab and then designate that you are working from that branch (using `git checkout`) or by selecting the branch in the RStudio git interface. At this point, you would work as you normally would. In this example, we could commit changes using the following code:

```
git add pull-request-information.md
git commit -m "Text for working on new branch and making test pull request."
git push origin my-branch
```

If you started the branch locally, it will now appear on GitLab. At some point, I will want to merge the branch back into the `master`, which can be done using a Pull Request. This is done on GitLab. You create a Pull Request by going to the repository on GitLab, and clicking on the `branches` icon. In this example, that is located here: [https://gitlab.moffitt.usf.edu:8000/ReproducibleResearch/MoffittFunctions/merge_requests](https://gitlab.moffitt.usf.edu:8000/ReproducibleResearch/MoffittFunctions/merge_requests). Once there, there should be a `New pull request` option to click on.

Once you click on that, there is an option to title the Pull Request and then write a comment about it. Once posted, information about the open pull request is readily available. There is an option on the side to create an "Assignee". This example has been closed so the branch is labeled "Merged" and cannot be edited or commented on anymore.

While the Pull request is open, comments can be made on the forum. The branch can also be pulled and edited by other people as well. New commites are tracked in the `Commits` tab. If multiple files are involved, those can be viewed in the `Files changed` tab.  The pull request is complete once `Merge pull request` is clicked, which will merge the request with the `master`. After that, the pull request can be closed.

After the merge, you may want to delete the branch, which can be done on GitLab. And lastly, you can switch back to the main branch using: 
```
git checkout master
```
then pull your updated master:
```
git pull origin master
```
and you are ready for next task.
