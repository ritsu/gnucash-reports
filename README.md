# gnucash-reports
A set of custom <a href="https://www.gnucash.org/">GnuCash</a> barchart and piechart reports that group accounts
based on tags. 

While this has been tested on my own system and appears to be working fine, accuracy of the reports is not
guaranteed, and there may be bugs. Testing, suggestions and code contributions are much appreciated!

## Motivation
GnuCash comes with a standard set of barchart and piechart reports that are already quite good. But like other
standard reports, individual balances (e.g. indvidual stacks in a stacked barchart) are constrained by the structure
of the account tree. That is, individual balances *must* consist of a single account or the combined balance of all
child accounts of a single account. 

For example, if you have multiple cash accounts spread across different branches in your account tree, you cannot
create a standard report that tracks them as one balance, unless they are all children of the same parent account,
and that parent account has no other non-cash accounts. 

## Introducing Tags
One way around this limitation is to allow tagging of accounts with (key: value) pairs. Reports could then use these 
tags to group accounts accordingly. For example, you could tag any number of accounts with `Type: Cash`, and no matter 
where they are in the acccount tree, their balances could be tracked together as one item in a report.

Since tags are not (yet) implemented in GnuCash, this report co-opts the **Account Note** field as the free-form input
of choice for user-defined tags. The report will interpret any comma-separated string containing a colon `:` as a tag's 
key-value pair. For example, suppose an account has the following **Account Note**:

  ```
  Type: Cash, Account Type: Savings, Institution: MyBank
  ```

* A tagged report using the tag-key `Type` will group this account with other `Type: Cash` accounts.
* A tagged report using the tag-key `Account Type` will group this account with other `Account Type: Savings` 
accounts. 
* A tagged report using the tag-key `Institution` will group this account with other `Institution: MyBank` 
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
Here, I am using `ritsu-reports`, but it can be anything.
* Clone the repo, or simply download all the `.scm` files to `USER_DATA_HOME/ritsu-reports/`.
* Create / edit the file `USER_DATA_HOME/config-user.scm` by adding the following lines:
  ```
  (load (gnc-build-userdata-path "ritsu-reports/tag-barchart-renderer.scm"))
  (load (gnc-build-userdata-path "ritsu-reports/tag-barchart-loader.scm"))
  (load (gnc-build-userdata-path "ritsu-reports/tag-piechart-renderer.scm"))
  (load (gnc-build-userdata-path "ritsu-reports/tag-piechart-loader.scm"))
  ```
* Restart GnuCash. There should now be reports called **Tagged ___ Report** under *Assets & Liabilities* and *Income & Expenses*.

## Options
The report options window includes a *Tag* tab for tag-related options. 

* **Group by**: Choose the tag-key by which the report will group accounts. The list of tag-keys are pulled from
*existing account notes*. If no tags are found, the report will simply show the total for all selected accounts.

    *Note: To refresh newly added tags (account notes) in the options window, the entire report needs to be closed and reopened.*

* **Custom sort**: Define a list of tag-values by which grouped balances will be sorted. *Sort Method* (in the Display
tab) needs to be set to *Custom* for this to have any effect.

* **Use parent tags as fallback**: If an account does not have the group-by tag in its account notes, the report
will use the first valid tag found from its parent accounts.

* **Display untagged balances**: When checked, the report will display the combined balance of all accounts for which
no valid tags could be found. When unchecked, those accounts will not be shown.

* **Normalize balances for each interval**: Display values as percentages instead of monetary amounts.

* **Display table of accounts by tag**: Show a table of all accounts and their tag values.

The tagged-barchart report also contains a *Chart* tab for transparency, tooltip behavior, and line chart options.

## License
<a href="https://www.gnu.org/licenses/gpl-3.0.en.html">GNU GPL</a>
