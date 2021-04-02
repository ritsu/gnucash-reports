# tag-barchart
A custom <a href="https://www.gnucash.org/">GnuCash</a> barchart report that groups accounts based on tags. 

While this has been tested on my own system and 
appears to be working fine, accuracy of the reports is not guaranteed, and there may be bugs. *I am not a Scheme 
coder.* Testing, suggestions and code contributions are much appreciated!

## Motivation
GnuCash comes with a standard set of barchart reports that are already quite good. But like other standard reports, 
individual balances (e.g. indvidual stacks in a stacked barchart) are constrained by the structure of the account tree.
That is, individual balances *must* consist of a single account or the combined balance of all child accounts of a single
account. 

For example, if you have multiple cash accounts spread across different branches in your account tree, you cannot create 
a standard report that tracks them as one balance, unless they are all children of the same parent account, and that parent 
account has no other non-cash accounts. 

## Introducing Tags
One way around this limitation is to allow tagging of accounts with (key: value) pairs. Reports could then use these 
tags to group accounts accordingly. For example, you could tag any number of accounts with `Type: Cash`, and no matter 
where they are in the acccount tree, their balances could be tracked together as one item in a report.

Since tags are not (yet) implemented in GnuCash, this report co-opts the **Account Note** field as the free-form input of 
choice for user-defined tags. The report will interpret any comma-separated string containing a colon `:` as a tag's 
key-value pair. For example, suppose an account has the following **Account Note**:

  ```
  Type: Cash, Account Type: Savings, Institution: MyBank
  ```

* A tag-barchart report using the tag-key `Type` will group this account with other `Type: Cash` accounts.
* A tag-barchart report using the tag-key `Account Type` will group this account with other `Account Type: Savings` 
accounts. 
* A tag-barchart report using the tag-key `Institution` will group this account with other `Institution: MyBank` 
accounts. 

## Examples
Suppose you have the following account tree (numbers and accounts are fictional):

![list-of-accounts](https://user-images.githubusercontent.com/490097/113089879-8e281900-91b6-11eb-8956-f84a9bb867db.png)

You can use tag-barchart to create a report for the different types of cash accounts: **Checking**, **Savings**, 
**Brokerage Sweep**, that are spread across multiple account tree branches.

<img src="https://user-images.githubusercontent.com/490097/113091034-13acc880-91b9-11eb-834e-41c07a82d233.png" width="700" />

Another feature of tag-barchart is the option to display balances as percentages of the total balance for each 
period. This makes it easy to see trends among tag groups over time. The options refer to this as *Normalized Balances*.

<img src="https://user-images.githubusercontent.com/490097/113091646-60dd6a00-91ba-11eb-9d98-ae3f45d5c205.png" width="500" /> <img src="https://user-images.githubusercontent.com/490097/113091655-65098780-91ba-11eb-890a-b3c8a328e079.png" width="500" />

<img src="https://user-images.githubusercontent.com/490097/113091663-6aff6880-91ba-11eb-9c86-cc3f461e10e6.png" width="500" /> <img src="https://user-images.githubusercontent.com/490097/113091670-6d61c280-91ba-11eb-9c26-2f6b7734e78d.png" width="500" />

## Installation
* Create a new directory in <a href="https://wiki.gnucash.org/wiki/Configuration_Locations#USER_DATA_HOME">USER_DATA_HOME</a>. 
Here, I am using `/ritsu-reports`, but it can be anything.
* Copy both `tag-barchart-loader.scm` and `tag-barchart-renderer.scm` to `/ritsu-reports`.
* Create / edit the file `config-user.scm` in <a href="https://wiki.gnucash.org/wiki/Configuration_Locations#USER_DATA_HOME">USER_DATA_HOME</a> 
by adding the following lines:
  ```
  (load (gnc-build-userdata-path "ritsu-reports/tag-barchart-renderer.scm"))
  (load (gnc-build-userdata-path "ritsu-reports/tag-barchart-loader.scm"))
  ```
* Restart GnuCash. There should now be reports called **Tagged ___ Report** under *Assets & Liabilities* and *Income & Expenses*.

## Notes
The report options window includes a tab for tag-related options. The **Group by** option allows the user to choose
which tag-key to use to group accounts. Available tag-keys are parsed from *existing account notes*. If there are no 
valid tags found in any account notes, no tag groups will be created, and the report will simply show the total for all 
selected accounts. 

To refresh newly added tags (account notes) ***in the options window***, the entire report needs to be closed and 
reopened. (That is, until someone can figure out a way to refresh the options window without reopening the report.)

Two separate files are used to load and render the report to facilitate development. They can be combined into a single 
file if you are comfortable working with Scheme. It may improve load times when reloading the report. 

## License
<a href="https://www.gnu.org/licenses/gpl-3.0.en.html">GNU GPL</a>
