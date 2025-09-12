window.onload = function() {
  console.log("HELLO")

  document.addEventListener("proposal-filters", (event) => {
    let views = document.querySelectorAll('[id^="ProposalCard"]')
    let filters = event.detail
    // console.log("FILTERS", filters)
    for (let view of views) {
      let hyperView = Hyperbole.hyperView(view.id)
      let action = Hyperbole.action("ProposalDetails", filters)
      hyperView.runAction(action)
    }
  })
}
