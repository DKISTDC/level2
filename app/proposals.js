window.onload = function() {
  console.log("proposals.js")
  
  const observer = new IntersectionObserver(entries => {
    for (const entry of entries) {
      if (entry.isIntersecting) {
        let view = entry.target
        if (view.dataset.loaded) {
          return
        }
        loadProposalDetails(entry.target)
      }
    }
  });

  // when filters change, reload the details of all loaded proposals
  document.addEventListener("proposal-filters", (event) => {
    console.log("FILTER CHANGE")
    for (let view of allProposals()) {
      if (view.dataset.loaded) {
        loadProposalDetails(view)
      }
    }
  })

  // scan all proposal cards and observe on load 
  for (let view of allProposals()) {
    observer.observe(view);
  }
}

function allProposals() {
  return document.querySelectorAll('[id^="ProposalCard"]')
}

function loadProposalDetails(view) {
  console.log('LOAD', view.id);
  view.dataset.loaded = true
  let hyperView = Hyperbole.hyperView(view.id)
  let action = Hyperbole.action("ProposalDetails")
  hyperView.runAction(action)
}
