import { sll_parser } from "./sll_parser.js"
import { residuator } from "./residuator.js"
import { supercompiler } from "./spsc.js"
import { samples } from "./samples.js"

function supercompile() {
    var src_code = document.getElementById('code').value;
    var src_goal = document.getElementById('goal').value;

    // console.log(src_code);

    var program = sll_parser.parse(src_code).result;
    var goal = sll_parser.parse_exp(src_goal);
    var sc = supercompiler(program);
    var tree = sc.build_tree(goal);

    var result = residuator(tree).residuate();

    // console.log('sc result:');
    document.getElementById('residual_goal').value = result[0].toString();
    document.getElementById('residual_code').value = result[1].toString();
}

function create_show_sample_handler(name) {
    return function () {
        var code = samples[name].rules.join('\n');
        var goal = samples[name].goal;
        document.getElementById('code').value = code;
        document.getElementById('goal').value = goal;
    };
}

var sample_links = document.getElementById('samples');
var link, li;
for (var name in samples) {
    li = document.createElement('li');
    link = document.createElement('a');
    link.appendChild(document.createTextNode(name));
    link.setAttribute('href', '#');
    link.addEventListener('click', create_show_sample_handler(name), false);
    li.appendChild(link);
    sample_links.appendChild(li);
}

const btn_sc = document.getElementById('btn_supercompile')
btn_sc.addEventListener("click", (_event) => {
    supercompile();
});
