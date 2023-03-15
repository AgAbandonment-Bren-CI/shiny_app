$(document).ready(function() {
  /**
    Custom slider labels
  **/

    // Convert numbers of min to max to "lower" and "higher"
    function returnLabels(value) {
      // remove label of selected
      $('.my_slider').find('.irs-single').remove();
    //  $('.my_slider').find('.irs-grid-text').remove(); // this is an alternative to ticks=F

      if (value === 1){ // enter your lowest slider value here
        return "Biodiversity";
      }else{
        return "Carbon";
      }
    }

    var someID = $("#feat_weight").ionRangeSlider({ // enter your shiny slider ID here
          prettify: returnLabels,
          force_edges: true,
          grid: false
        });
    });