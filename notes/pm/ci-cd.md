# Continuous Integration
The practice of merging developer working copies to a shared *mainline* several times a day.

![cd-diagram](https://upload.wikimedia.org/wikipedia/commons/thumb/c/c3/Continuous_Delivery_process_diagram.svg/731px-Continuous_Delivery_process_diagram.svg.png)

In XP, CI is used in combination with *automated unit tests* through test-driven development. This is done through developer's local environment. Partially complete features can be disabled before committing, using [feature toggle](https://en.wikipedia.org/wiki/Feature_toggle) for instance.

The basic idea is to have a configuration file

Feature toggle can be categorized into two types:
1. Release toggle, developer determines to either keep/remove before a product release depending on its working.
2. Business toggle,


Build server is typically used to implement continuous processes of applying quality control in general. Measure profile performance, extract and format documentation from the source code and faciilitate manual QA processes. This part aims at improving the quality of software, and reduce time to deliver it.

QA process and development steps

- Review of requirements
  - completeness
  - redundancies
  - clarity
  - consistency
  - executability
  - verifiability
- Test planning / writing test cases
- Unit testing
- Integration testing
- System testing
- Performance testing
   - Load testing, stress testing
- Security testing
- Cross-browser testing / cross-platform testing
- Updating test cases
- Regression testing
