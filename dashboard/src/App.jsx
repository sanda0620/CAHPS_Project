import { useState, useEffect } from "react";
import Papa from "papaparse";
import {
  BarChart, Bar, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer,
  Cell, RadarChart, Radar, PolarGrid, PolarAngleAxis, ReferenceLine, Legend
} from "recharts";

// ── Responsive CSS injected once ─────────────────────────────────────────────
const STYLES = `
  *, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }
  body { background: #0f172a; color: #f1f5f9; font-family: 'DM Sans','Segoe UI',sans-serif; }
  .grid-2 { display: grid; grid-template-columns: 1fr 1fr; gap: 22px; }
  .grid-4 { display: grid; grid-template-columns: repeat(4, 1fr); gap: 14px; }
  .flex-kpi { display: flex; gap: 14px; flex-wrap: wrap; }
  .flex-kpi > * { flex: 1; min-width: 140px; }
  .tabs-nav { display: flex; gap: 4px; overflow-x: auto; scrollbar-width: none; }
  .tabs-nav::-webkit-scrollbar { display: none; }
  .tab-btn { white-space: nowrap; background: none; border: none; cursor: pointer;
    padding: 14px 18px; font-size: 14px; font-weight: 600; transition: all 0.15s;
    letter-spacing: 0.01em; font-family: inherit; }
  .content-wrap { max-width: 1100px; margin: 0 auto; padding: 36px 24px 0; }
  .header-wrap { max-width: 1100px; margin: 0 auto; }
  @media (max-width: 768px) {
    .grid-2 { grid-template-columns: 1fr; }
    .grid-4 { grid-template-columns: 1fr 1fr; }
    .content-wrap { padding: 20px 16px 0; }
    h1.dash-title { font-size: 20px !important; }
    .tab-btn { padding: 12px 14px; font-size: 13px; }
    .stat-val { font-size: 22px !important; }
    .header-pad { padding: 20px 16px 18px !important; }
    .nav-pad { padding: 0 16px !important; }
  }
  @media (max-width: 480px) {
    .grid-4 { grid-template-columns: 1fr 1fr; }
    .flex-kpi > * { min-width: 120px; }
  }
`;

// ── CSV loader ────────────────────────────────────────────────────────────────
function useCSV(filename) {
  const [data, setData] = useState([]);
  const [loading, setLoading] = useState(true);
  useEffect(() => {
    Papa.parse(`/${filename}`, {
      download: true, header: true, dynamicTyping: true, skipEmptyLines: true,
      complete: (r) => { setData(r.data); setLoading(false); },
      error: () => setLoading(false),
    });
  }, [filename]);
  return { data, loading };
}

// ── Palette ───────────────────────────────────────────────────────────────────
const C = {
  never: "#94a3b8", sometimes: "#60a5fa", usually: "#3b82f6", always: "#1d4ed8",
  accent: "#f59e0b", danger: "#ef4444", success: "#10b981",
  bg: "#0f172a", card: "#1e293b", border: "#334155", text: "#f1f5f9", textMuted: "#94a3b8",
};
const LISTEN_COLORS = { Never: C.never, Sometimes: C.sometimes, Usually: C.usually, Always: C.always };
const LISTEN_ORDER  = ["Never", "Sometimes", "Usually", "Always"];

// ── Static data (from R — not in CSVs) ───────────────────────────────────────
const corrMatrix = {
  vars: ["Listening","Expl.","Respect","Time","HP Rtg","Dr Rtg","HC Rtg","Spec.Rtg","Comm.Score","Hlth"],
  values: [
    [1.00,0.51,0.49,0.48,0.01,0.02,-0.04,0.01,0.72,0.00],
    [0.51,1.00,0.50,0.49,0.01,0.02,-0.03,0.01,0.80,-0.01],
    [0.49,0.50,1.00,0.51,0.01,0.01,-0.03,0.01,0.79,0.00],
    [0.48,0.49,0.51,1.00,0.01,0.02,-0.03,0.01,0.79,0.00],
    [0.01,0.01,0.01,0.01,1.00,0.00,-0.01,0.00,0.01,0.01],
    [0.02,0.02,0.01,0.02,0.00,1.00,0.01,0.01,0.02,0.02],
    [-0.04,-0.03,-0.03,-0.03,-0.01,0.01,1.00,0.00,-0.04,0.01],
    [0.01,0.01,0.01,0.01,0.00,0.01,0.00,1.00,0.01,0.00],
    [0.72,0.80,0.79,0.79,0.01,0.02,-0.04,0.01,1.00,0.00],
    [0.00,-0.01,0.00,0.00,0.01,0.02,0.01,0.00,0.00,1.00],
  ]
};
const corrData = [
  { outcome: "HP Rating",    rho:  0.013, p: 0.465, sig: false },
  { outcome: "Doctor Rtg",   rho:  0.021, p: 0.248, sig: false },
  { outcome: "HC Rating",    rho: -0.038, p: 0.037, sig: true  },
  { outcome: "Spec. Rating", rho:  0.009, p: 0.621, sig: false },
];
const coefData = [
  { name: "Doctor Listening",   est:  0.041, sig: false },
  { name: "Doctor Explanation", est:  0.028, sig: false },
  { name: "Doctor Respect",     est:  0.019, sig: false },
  { name: "Time Spent",         est:  0.012, sig: false },
  { name: "Doctor Rating",      est:  0.008, sig: false },
  { name: "Overall Health",     est:  0.015, sig: false },
  { name: "Age Group",          est: -0.011, sig: false },
  { name: "Gender (Female)",    est:  0.027, sig: false },
  { name: "Education Level",    est:  0.003, sig: false },
].sort((a, b) => a.est - b.est);
const vifData = [
  { name: "Doctor Listening",   vif: 1.58 },
  { name: "Doctor Explanation", vif: 1.57 },
  { name: "Doctor Respect",     vif: 1.57 },
  { name: "Time Spent",         vif: 1.57 },
  { name: "Doctor Rating",      vif: 1.01 },
  { name: "Overall Health",     vif: 1.01 },
  { name: "Age Group",          vif: 1.02 },
  { name: "Gender",             vif: 1.01 },
  { name: "Education",          vif: 1.01 },
];

// ── Shared UI ─────────────────────────────────────────────────────────────────
const heatColor = (v) => {
  if (v >= 0.6)  return "#1d4ed8";
  if (v >= 0.3)  return "#3b82f6";
  if (v >= 0.1)  return "#93c5fd";
  if (v > -0.1)  return "#1e293b";
  if (v > -0.3)  return "#fca5a5";
  return "#ef4444";
};

const SigBadge = ({ sig }) => (
  <span style={{
    display:"inline-block", padding:"2px 8px", borderRadius:4, fontSize:11,
    fontWeight:700, fontFamily:"monospace",
    background: sig ? "#ef444422" : "#10b98122",
    color: sig ? "#fca5a5" : "#6ee7b7",
    border: `1px solid ${sig ? "#ef4444" : "#10b981"}44`,
  }}>{sig ? "p<0.05 ✓" : "n.s."}</span>
);

const StatCard = ({ label, value, sub, color }) => (
  <div style={{ background:C.card, border:`1px solid ${C.border}`, borderRadius:12, padding:"18px 22px" }}>
    <div style={{ fontSize:11, color:C.textMuted, letterSpacing:"0.08em", textTransform:"uppercase", marginBottom:6 }}>{label}</div>
    <div className="stat-val" style={{ fontSize:28, fontWeight:800, color:color||C.text, letterSpacing:"-0.02em" }}>{value}</div>
    {sub && <div style={{ fontSize:12, color:C.textMuted, marginTop:4 }}>{sub}</div>}
  </div>
);

const Card = ({ children, style }) => (
  <div style={{ background:C.card, border:`1px solid ${C.border}`, borderRadius:14, padding:24, ...style }}>
    {children}
  </div>
);

const ChartLabel = ({ children }) => (
  <div style={{ fontSize:13, fontWeight:600, color:C.textMuted, marginBottom:14, letterSpacing:"0.02em" }}>{children}</div>
);

const DarkTooltip = ({ active, payload, label }) => {
  if (!active || !payload?.length) return null;
  return (
    <div style={{ background:"#0f172a", border:`1px solid ${C.border}`, borderRadius:8, padding:"10px 14px", fontSize:13 }}>
      <div style={{ color:C.textMuted, marginBottom:4 }}>{label}</div>
      {payload.map((p,i) => (
        <div key={i} style={{ color:p.color||C.text }}>
          {p.name}: <strong>{typeof p.value==="number" ? p.value.toFixed(3) : p.value}</strong>
        </div>
      ))}
    </div>
  );
};

const Spinner = () => (
  <div style={{ color:C.textMuted, padding:"32px 0", textAlign:"center", fontSize:13 }}>Loading data…</div>
);

const TABS = [
  { id:"overview",    label:"Overview" },
  { id:"descriptive", label:"Descriptive" },
  { id:"inferential", label:"Inferential" },
  { id:"predictive",  label:"Predictive" },
  { id:"heatmap",     label:"Correlation" },
];

// ══════════════════════════════════════════════════════════════════════════════
// OVERVIEW
// ══════════════════════════════════════════════════════════════════════════════
function OverviewTab({ infData }) {
  const kwRow = infData.find(r => r.test?.includes("Kruskal") && r.test?.includes("HPR"));
  const spRow = infData.find(r => r.test?.includes("Spearman"));
  const kwP   = kwRow ? kwRow.p_value?.toFixed(3) : "0.698";
  const spRho = spRow ? `+${spRow.statistic?.toFixed(3)}` : "+0.013";

  return (
    <>
      <div className="flex-kpi" style={{ marginBottom:32 }}>
        <StatCard label="Total Respondents"    value="3,000" sub="36 columns · 0 duplicates" />
        <StatCard label="Missing (analytical)" value="0"     sub="3 visit-count cols excluded"  color={C.success} />
        <StatCard label="Mean HP Rating"       value="5.01"  sub="out of 10 (SD = 3.10)"        color={C.accent} />
        <StatCard label="High Satisfaction"    value="36%"   sub="Health Plan Rating ≥ 7"       color="#60a5fa" />
        <StatCard label="Kruskal-Wallis p"     value={kwP}   sub="NOT significant"               color={C.danger} />
      </div>

      <Card style={{ marginBottom:28, borderColor:"#ef444433" }}>
        <div style={{ fontSize:13, color:C.accent, fontWeight:700, letterSpacing:"0.08em", textTransform:"uppercase", marginBottom:12 }}>Key Conclusion</div>
        <div className="grid-2">
          {[
            { label:"Descriptive",    icon:"📊", val:`Mean HP Rating ranges only 4.89–5.08 across all listening groups (<0.2pt difference)` },
            { label:"Spearman ρ",     icon:"📈", val:`Listening vs HP Rating: ρ = ${spRho}, p = 0.465 — essentially zero correlation` },
            { label:"Kruskal-Wallis", icon:"🔬", val:`H = 1.43, df = 3, p = ${kwP} — groups are statistically identical` },
            { label:"Regression R²",  icon:"📉", val:`Communication variables explain ~0% of variance in satisfaction (R² ≈ 0.000)` },
          ].map(item => (
            <div key={item.label} style={{ background:"#0f172a", borderRadius:10, padding:"14px 16px", borderLeft:`3px solid ${C.border}` }}>
              <div style={{ fontSize:12, color:C.textMuted, marginBottom:4 }}>{item.icon} {item.label}</div>
              <div style={{ fontSize:13.5, color:C.text }}>{item.val}</div>
            </div>
          ))}
        </div>
      </Card>

      <Card>
        <ChartLabel>Data Quality Summary</ChartLabel>
        <div className="grid-4">
          {[
            { label:"Duplicate rows",            val:"0",   good:true },
            { label:"Whitespace issues",          val:"0",   good:true },
            { label:"Out-of-range ratings",       val:"0",   good:true },
            { label:"Missing in analysis vars",   val:"0",   good:true },
            { label:"Health_Care_Visits missing", val:"407", good:false },
            { label:"Doctor_Visits missing",      val:"441", good:false },
            { label:"Specialist_Visits missing",  val:"500", good:false },
            { label:"Ordinal cols standardised",  val:"10",  good:true },
          ].map(item => (
            <div key={item.label} style={{ background:"#0f172a", borderRadius:8, padding:"12px 14px" }}>
              <div style={{ fontSize:20, fontWeight:800, color:item.good ? C.success : C.accent }}>{item.val}</div>
              <div style={{ fontSize:12, color:C.textMuted, marginTop:4 }}>{item.label}</div>
            </div>
          ))}
        </div>
      </Card>
    </>
  );
}

// ══════════════════════════════════════════════════════════════════════════════
// DESCRIPTIVE
// ══════════════════════════════════════════════════════════════════════════════
function DescriptiveTab({ listenData, ratingData }) {
  const sortedListen  = [...listenData].sort((a,b) => LISTEN_ORDER.indexOf(a.group) - LISTEN_ORDER.indexOf(b.group));
  const sortedRatings = [...ratingData].sort((a,b) => LISTEN_ORDER.indexOf(a.Doctor_Listening) - LISTEN_ORDER.indexOf(b.Doctor_Listening));

  const grouped = sortedRatings.map(r => ({
    level: r.Doctor_Listening,
    "HP Rating":  r.mean_HPR,
    "Doctor Rtg": r.mean_DR,
    "HC Rating":  r.mean_HCR,
  }));

  const radarData = [
    { metric:"High Sat %",  ...Object.fromEntries(sortedRatings.map(r=>[r.Doctor_Listening, r.high_sat])) },
    { metric:"Mean HPR×10", ...Object.fromEntries(sortedRatings.map(r=>[r.Doctor_Listening, +(r.mean_HPR*10).toFixed(1)])) },
    { metric:"Mean DR×10",  ...Object.fromEntries(sortedRatings.map(r=>[r.Doctor_Listening, +(r.mean_DR*10).toFixed(1)])) },
    { metric:"Mean HCR×10", ...Object.fromEntries(sortedRatings.map(r=>[r.Doctor_Listening, +(r.mean_HCR*10).toFixed(1)])) },
  ];

  return (
    <>
      <div className="grid-2" style={{ marginBottom:22 }}>
        <Card>
          <ChartLabel>Fig 4 · Doctor Listening Distribution (n = 3,000)</ChartLabel>
          {listenData.length===0 ? <Spinner/> : (
            <>
              <ResponsiveContainer width="100%" height={220}>
                <BarChart data={sortedListen} barSize={44}>
                  <CartesianGrid strokeDasharray="3 3" stroke={C.border} vertical={false} />
                  <XAxis dataKey="group" tick={{ fill:C.textMuted, fontSize:12 }} axisLine={false} tickLine={false} />
                  <YAxis tick={{ fill:C.textMuted, fontSize:12 }} axisLine={false} tickLine={false} domain={[0,900]} />
                  <Tooltip content={<DarkTooltip/>} />
                  <Bar dataKey="count" name="Respondents" radius={[5,5,0,0]}>
                    {sortedListen.map((d,i) => <Cell key={i} fill={LISTEN_COLORS[d.group]||C.muted} />)}
                  </Bar>
                </BarChart>
              </ResponsiveContainer>
              <div style={{ display:"flex", gap:10, marginTop:10, flexWrap:"wrap" }}>
                {sortedListen.map(d => (
                  <span key={d.group} style={{ fontSize:12, color:C.textMuted }}>
                    <span style={{ color:LISTEN_COLORS[d.group] }}>●</span> {d.group}: {d.count ? ((d.count/3000)*100).toFixed(1) : "—"}%
                  </span>
                ))}
              </div>
            </>
          )}
        </Card>

        <Card>
          <ChartLabel>Fig 2 · High Satisfaction Rate (HPR ≥ 7) by Listening Level</ChartLabel>
          {ratingData.length===0 ? <Spinner/> : (
            <ResponsiveContainer width="100%" height={220}>
              <BarChart data={sortedRatings} barSize={44}>
                <CartesianGrid strokeDasharray="3 3" stroke={C.border} vertical={false} />
                <XAxis dataKey="Doctor_Listening" tick={{ fill:C.textMuted, fontSize:12 }} axisLine={false} tickLine={false} />
                <YAxis tick={{ fill:C.textMuted, fontSize:12 }} axisLine={false} tickLine={false} domain={[0,50]} tickFormatter={v=>`${v}%`} />
                <Tooltip content={<DarkTooltip/>} formatter={v=>`${v}%`} />
                <Bar dataKey="high_sat" name="High Sat %" radius={[5,5,0,0]}>
                  {sortedRatings.map((d,i) => <Cell key={i} fill={LISTEN_COLORS[d.Doctor_Listening]||C.muted} />)}
                </Bar>
              </BarChart>
            </ResponsiveContainer>
          )}
        </Card>
      </div>

      <Card style={{ marginBottom:22 }}>
        <ChartLabel>Fig 1 · Mean Ratings by Doctor Listening Level — nearly identical across all groups</ChartLabel>
        {ratingData.length===0 ? <Spinner/> : (
          <>
            <ResponsiveContainer width="100%" height={240}>
              <BarChart data={grouped} barSize={18}>
                <CartesianGrid strokeDasharray="3 3" stroke={C.border} vertical={false} />
                <XAxis dataKey="level" tick={{ fill:C.textMuted, fontSize:12 }} axisLine={false} tickLine={false} />
                <YAxis tick={{ fill:C.textMuted, fontSize:12 }} axisLine={false} tickLine={false} domain={[4.5,5.5]} />
                <Tooltip content={<DarkTooltip/>} />
                <Legend wrapperStyle={{ fontSize:12, color:C.textMuted }} />
                <Bar dataKey="HP Rating"  fill="#3b82f6" radius={[4,4,0,0]} />
                <Bar dataKey="Doctor Rtg" fill="#60a5fa" radius={[4,4,0,0]} />
                <Bar dataKey="HC Rating"  fill="#93c5fd" radius={[4,4,0,0]} />
              </BarChart>
            </ResponsiveContainer>
            <div style={{ marginTop:14, padding:"10px 14px", background:"#0f172a", borderRadius:8, fontSize:13, color:C.textMuted }}>
              ⚠ Max difference in mean HP Rating across listening groups: <strong style={{ color:C.text }}>0.19 points</strong> on a 0–10 scale — practically negligible.
            </div>
          </>
        )}
      </Card>

      <Card style={{ marginBottom:22 }}>
        <ChartLabel>Group-Level Descriptive Statistics Table</ChartLabel>
        {ratingData.length===0 ? <Spinner/> : (
          <div style={{ overflowX:"auto" }}>
            <table style={{ width:"100%", borderCollapse:"collapse", fontSize:13, minWidth:500 }}>
              <thead>
                <tr style={{ borderBottom:`1px solid ${C.border}` }}>
                  {["Listening Level","n","Mean HPR","Mean DR","Mean HCR","High Sat %"].map(h => (
                    <th key={h} style={{ textAlign:"left", padding:"8px 12px", color:C.textMuted, fontWeight:600, fontSize:12 }}>{h}</th>
                  ))}
                </tr>
              </thead>
              <tbody>
                {sortedRatings.map((row,i) => (
                  <tr key={row.Doctor_Listening} style={{ borderBottom:`1px solid ${C.border}22`, background:i%2===0?"#ffffff08":"transparent" }}>
                    <td style={{ padding:"10px 12px" }}><span style={{ color:LISTEN_COLORS[row.Doctor_Listening], marginRight:8 }}>●</span>{row.Doctor_Listening}</td>
                    <td style={{ padding:"10px 12px", color:C.textMuted }}>{row.n}</td>
                    <td style={{ padding:"10px 12px", fontWeight:600 }}>{row.mean_HPR?.toFixed(2)}</td>
                    <td style={{ padding:"10px 12px" }}>{row.mean_DR?.toFixed(2)}</td>
                    <td style={{ padding:"10px 12px" }}>{row.mean_HCR?.toFixed(2)}</td>
                    <td style={{ padding:"10px 12px", fontWeight:600, color:C.accent }}>{row.high_sat?.toFixed(1)}%</td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        )}
      </Card>

      <Card>
        <ChartLabel>Multi-metric Radar — all groups overlap tightly</ChartLabel>
        {ratingData.length===0 ? <Spinner/> : (
          <ResponsiveContainer width="100%" height={300}>
            <RadarChart data={radarData} cx="50%" cy="50%" outerRadius={110}>
              <PolarGrid stroke={C.border} />
              <PolarAngleAxis dataKey="metric" tick={{ fill:C.textMuted, fontSize:12 }} />
              {LISTEN_ORDER.map(g => (
                <Radar key={g} name={g} dataKey={g} stroke={LISTEN_COLORS[g]} fill={LISTEN_COLORS[g]} fillOpacity={0.08} />
              ))}
              <Legend wrapperStyle={{ fontSize:12, color:C.textMuted }} />
              <Tooltip content={<DarkTooltip/>} />
            </RadarChart>
          </ResponsiveContainer>
        )}
      </Card>
    </>
  );
}

// ══════════════════════════════════════════════════════════════════════════════
// INFERENTIAL
// ══════════════════════════════════════════════════════════════════════════════
function InferentialTab({ infData }) {
  return (
    <>
      <div className="grid-2" style={{ marginBottom:22 }}>
        <Card>
          <ChartLabel>Spearman Rank Correlation — Listening vs Outcomes</ChartLabel>
          <ResponsiveContainer width="100%" height={200}>
            <BarChart data={corrData} layout="vertical" barSize={18}>
              <CartesianGrid strokeDasharray="3 3" stroke={C.border} horizontal={false} />
              <XAxis type="number" domain={[-0.08,0.06]} tick={{ fill:C.textMuted, fontSize:11 }} axisLine={false} tickLine={false} tickFormatter={v=>v.toFixed(2)} />
              <YAxis type="category" dataKey="outcome" tick={{ fill:C.textMuted, fontSize:12 }} axisLine={false} tickLine={false} width={80} />
              <Tooltip content={<DarkTooltip/>} />
              <ReferenceLine x={0} stroke={C.border} />
              <Bar dataKey="rho" name="Spearman ρ" radius={[0,4,4,0]}>
                {corrData.map((d,i) => <Cell key={i} fill={d.rho<0 ? C.danger : (d.sig ? C.accent : "#3b82f6")} />)}
              </Bar>
            </BarChart>
          </ResponsiveContainer>
          <div style={{ marginTop:14 }}>
            {corrData.map(d => (
              <div key={d.outcome} style={{ display:"flex", justifyContent:"space-between", alignItems:"center", padding:"6px 0", borderBottom:`1px solid ${C.border}22`, flexWrap:"wrap", gap:8 }}>
                <span style={{ fontSize:13 }}>{d.outcome}</span>
                <div style={{ display:"flex", gap:10, alignItems:"center", flexWrap:"wrap" }}>
                  <span style={{ fontSize:13, fontFamily:"monospace", color:d.rho<0?C.danger:C.text }}>ρ={d.rho>=0?"+":""}{d.rho.toFixed(3)}</span>
                  <span style={{ fontSize:12, color:C.textMuted }}>p={d.p}</span>
                  <SigBadge sig={d.sig} />
                </div>
              </div>
            ))}
          </div>
        </Card>

        <Card>
          <ChartLabel>Statistical Tests — from inferential.csv</ChartLabel>
          {infData.length===0 ? <Spinner/> : (
            <>
              <ResponsiveContainer width="100%" height={200}>
                <BarChart data={infData} layout="vertical" barSize={18}>
                  <CartesianGrid strokeDasharray="3 3" stroke={C.border} horizontal={false} />
                  <XAxis type="number" tick={{ fill:C.textMuted, fontSize:11 }} axisLine={false} tickLine={false} />
                  <YAxis type="category" dataKey="test" tick={{ fill:C.textMuted, fontSize:10 }} axisLine={false} tickLine={false} width={140} />
                  <Tooltip content={<DarkTooltip/>} />
                  <ReferenceLine x={5.991} stroke={C.accent} strokeDasharray="4 2" label={{ value:"χ²₃ crit.", fill:C.accent, fontSize:10 }} />
                  <Bar dataKey="statistic" name="Test Statistic" radius={[0,4,4,0]}>
                    {infData.map((d,i) => <Cell key={i} fill={d.significant ? C.accent : "#3b82f6"} />)}
                  </Bar>
                </BarChart>
              </ResponsiveContainer>
              <div style={{ marginTop:14 }}>
                {infData.map(d => (
                  <div key={d.test} style={{ display:"flex", justifyContent:"space-between", alignItems:"center", padding:"6px 0", borderBottom:`1px solid ${C.border}22`, flexWrap:"wrap", gap:8 }}>
                    <span style={{ fontSize:12, color:C.textMuted, maxWidth:200 }}>{d.test}</span>
                    <div style={{ display:"flex", gap:8, alignItems:"center" }}>
                      <span style={{ fontSize:13, fontFamily:"monospace" }}>{d.statistic?.toFixed(3)}</span>
                      <span style={{ fontSize:12, color:C.textMuted }}>p={d.p_value?.toFixed(3)}</span>
                      <SigBadge sig={!!d.significant} />
                    </div>
                  </div>
                ))}
              </div>
            </>
          )}
        </Card>
      </div>

      <Card>
        <ChartLabel>Assumption Testing Results</ChartLabel>
        <div className="grid-2">
          {[
            { test:"Shapiro-Wilk Normality Test", result:"W = 0.9641, p < 0.0001", conclusion:"NOT normally distributed → Non-parametric tests used", warn:true },
            { test:"Levene's Homogeneity of Variance", result:"F(3,2996) ≈ 0.12, p ≈ 0.95", conclusion:"Equal variances across listening groups — assumption met", warn:false },
            { test:"MCAR Test (Health_Care_Visits)", result:"t-test: p > 0.05", conclusion:"Missing at Random (MAR) — acceptable, not used in analysis", warn:false },
            { test:"CAHPS Skip-Logic Check", result:"Some respondents answered filtered questions", conclusion:"Known CAHPS survey artefact — rows kept, flagged only", warn:false },
          ].map(item => (
            <div key={item.test} style={{ background:"#0f172a", borderRadius:10, padding:"14px 16px", borderLeft:`3px solid ${item.warn?C.accent:C.success}` }}>
              <div style={{ fontSize:12, fontWeight:700, color:item.warn?C.accent:C.success, marginBottom:4 }}>{item.test}</div>
              <div style={{ fontSize:13, fontFamily:"monospace", color:C.text, marginBottom:6 }}>{item.result}</div>
              <div style={{ fontSize:12, color:C.textMuted }}>{item.conclusion}</div>
            </div>
          ))}
        </div>
      </Card>
    </>
  );
}

// ══════════════════════════════════════════════════════════════════════════════
// PREDICTIVE
// ══════════════════════════════════════════════════════════════════════════════
function PredictiveTab({ modData }) {
  return (
    <>
      <div className="flex-kpi" style={{ marginBottom:22 }}>
        {modData.length>0 ? modData.map(m => (
          <StatCard key={m.model} label={m.model} value={m.r2_test?.toFixed(4)??"0.000"} sub={`RMSE=${m.rmse?.toFixed(2)} · CV R²=${m.cv_r2?.toFixed(3)}`} color={C.danger} />
        )) : (
          <>
            <StatCard label="Model 1 Test R²" value="0.000" sub="Communication only" color={C.danger} />
            <StatCard label="Model 2 Test R²" value="0.000" sub="Full model"          color={C.danger} />
          </>
        )}
        <StatCard label="CV R² (5-fold)" value="−0.001" sub="No predictive power" color={C.danger} />
        <StatCard label="RMSE (both)"    value="3.11"   sub="Points on 0–10 scale" color={C.textMuted} />
      </div>

      <Card style={{ marginBottom:22 }}>
        <ChartLabel>Fig 7 · Regression Coefficients — Full Model (Model 2)</ChartLabel>
        <ResponsiveContainer width="100%" height={280}>
          <BarChart data={coefData} layout="vertical" barSize={14}>
            <CartesianGrid strokeDasharray="3 3" stroke={C.border} horizontal={false} />
            <XAxis type="number" tick={{ fill:C.textMuted, fontSize:11 }} axisLine={false} tickLine={false} tickFormatter={v=>v.toFixed(2)} domain={[-0.15,0.18]} />
            <YAxis type="category" dataKey="name" tick={{ fill:C.textMuted, fontSize:12 }} axisLine={false} tickLine={false} width={130} />
            <Tooltip content={<DarkTooltip/>} />
            <ReferenceLine x={0} stroke={C.border} />
            <Bar dataKey="est" name="Coefficient" radius={[0,4,4,0]}>
              {coefData.map((d,i) => <Cell key={i} fill={d.sig?C.danger:"#3b82f6"} />)}
            </Bar>
          </BarChart>
        </ResponsiveContainer>
        <div style={{ marginTop:12, padding:"10px 14px", background:"#0f172a", borderRadius:8, fontSize:13, color:C.textMuted }}>
          All 9 predictor coefficients are <strong style={{ color:C.text }}>not statistically significant</strong> (all p &gt; 0.05). Communication variables have no predictive power.
        </div>
      </Card>

      <div className="grid-2">
        <Card>
          <ChartLabel>Model Performance (from models.csv)</ChartLabel>
          {modData.length===0 ? <Spinner/> : (
            <div style={{ overflowX:"auto" }}>
              <table style={{ width:"100%", borderCollapse:"collapse", fontSize:13, minWidth:300 }}>
                <thead>
                  <tr style={{ borderBottom:`1px solid ${C.border}` }}>
                    {["Metric","Model 1","Model 2"].map(h => (
                      <th key={h} style={{ textAlign:"left", padding:"8px 10px", color:C.textMuted, fontWeight:600, fontSize:12 }}>{h}</th>
                    ))}
                  </tr>
                </thead>
                <tbody>
                  {[
                    ["Test R²",       modData[0]?.r2_test?.toFixed(4), modData[1]?.r2_test?.toFixed(4)],
                    ["RMSE",          modData[0]?.rmse?.toFixed(3),    modData[1]?.rmse?.toFixed(3)],
                    ["CV R² (5-fold)",modData[0]?.cv_r2?.toFixed(3),   modData[1]?.cv_r2?.toFixed(3)],
                    ["AIC",           modData[0]?.aic?.toFixed(1),     modData[1]?.aic?.toFixed(1)],
                  ].map(([metric,m1,m2],i) => (
                    <tr key={metric} style={{ borderBottom:`1px solid ${C.border}22`, background:i%2===0?"#ffffff08":"transparent" }}>
                      <td style={{ padding:"9px 10px", color:C.textMuted }}>{metric}</td>
                      <td style={{ padding:"9px 10px", fontFamily:"monospace", color:C.danger }}>{m1??"—"}</td>
                      <td style={{ padding:"9px 10px", fontFamily:"monospace", color:C.danger }}>{m2??"—"}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          )}
        </Card>

        <Card>
          <ChartLabel>VIF Check — Multicollinearity (Model 2)</ChartLabel>
          {vifData.map(d => (
            <div key={d.name} style={{ display:"flex", alignItems:"center", gap:10, marginBottom:8 }}>
              <div style={{ width:120, fontSize:12, color:C.textMuted, flexShrink:0 }}>{d.name}</div>
              <div style={{ flex:1, height:8, background:"#0f172a", borderRadius:4, overflow:"hidden" }}>
                <div style={{ height:"100%", width:`${(d.vif/5)*100}%`, background:d.vif>5?C.danger:d.vif>2?C.accent:C.success, borderRadius:4 }} />
              </div>
              <div style={{ fontSize:12, fontFamily:"monospace", color:C.text, width:36 }}>{d.vif}</div>
            </div>
          ))}
          <div style={{ fontSize:12, color:C.textMuted, marginTop:10 }}>All VIF &lt; 5 — no multicollinearity concern.</div>
        </Card>
      </div>
    </>
  );
}

// ══════════════════════════════════════════════════════════════════════════════
// CORRELATION / HEATMAP
// ══════════════════════════════════════════════════════════════════════════════
function HeatmapTab({ corrCsvData }) {
  const lookup = {};
  corrCsvData.forEach(r => { lookup[`${r.var1}||${r.var2}`] = r.rho; });

  const SHORT = {
    Health_Plan_Rating:"HP Rtg", Doctor_Rating:"Dr Rtg",
    Listening_Num:"Listening",   Explanation_Num:"Expl.",
    Respect_Num:"Respect",       Time_Num:"Time",
  };
  const csvVars = Object.keys(SHORT);

  return (
    <>
      {corrCsvData.length>0 && (
        <Card style={{ marginBottom:22 }}>
          <ChartLabel>Spearman Correlation — Key Variables (live from correlation.csv)</ChartLabel>
          <div style={{ overflowX:"auto" }}>
            <table style={{ borderCollapse:"separate", borderSpacing:3, fontSize:11 }}>
              <thead>
                <tr>
                  <td style={{ width:72 }} />
                  {csvVars.map(v => <th key={v} style={{ fontSize:10, color:C.textMuted, fontWeight:600, padding:"4px 6px", textAlign:"center" }}>{SHORT[v]}</th>)}
                </tr>
              </thead>
              <tbody>
                {csvVars.map(rv => (
                  <tr key={rv}>
                    <td style={{ fontSize:10, color:C.textMuted, textAlign:"right", paddingRight:6, whiteSpace:"nowrap" }}>{SHORT[rv]}</td>
                    {csvVars.map(cv => {
                      const val = lookup[`${rv}||${cv}`] ?? lookup[`${cv}||${rv}`] ?? null;
                      return (
                        <td key={cv} style={{ width:60, height:42, textAlign:"center", background:val!==null?heatColor(val):C.card, borderRadius:4, fontFamily:"monospace", fontSize:10, color:val!==null&&Math.abs(val)>0.4?"#fff":C.text }}>
                          {val!==null ? val.toFixed(2) : "—"}
                        </td>
                      );
                    })}
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
          <div style={{ display:"flex", alignItems:"center", gap:6, marginTop:16, flexWrap:"wrap" }}>
            <span style={{ fontSize:11, color:C.textMuted }}>−1.0</span>
            {["#ef4444","#fca5a5","#1e293b","#93c5fd","#3b82f6","#1d4ed8"].map((c,i) => (
              <div key={i} style={{ width:30, height:16, background:c, borderRadius:3 }} />
            ))}
            <span style={{ fontSize:11, color:C.textMuted }}>+1.0</span>
          </div>
        </Card>
      )}

      <Card style={{ marginBottom:22 }}>
        <ChartLabel>Fig 5 · Full 10-Variable Spearman Correlation Matrix</ChartLabel>
        <div style={{ overflowX:"auto" }}>
          <table style={{ borderCollapse:"separate", borderSpacing:3, fontSize:11 }}>
            <thead>
              <tr>
                <td style={{ width:72 }} />
                {corrMatrix.vars.map(v => (
                  <th key={v} style={{ fontSize:10, color:C.textMuted, fontWeight:600, padding:"4px 2px", textAlign:"center", maxWidth:60 }}>
                    <div style={{ width:60, textAlign:"right", paddingRight:4 }}>{v}</div>
                  </th>
                ))}
              </tr>
            </thead>
            <tbody>
              {corrMatrix.vars.map((rowVar,ri) => (
                <tr key={rowVar}>
                  <td style={{ fontSize:10, color:C.textMuted, textAlign:"right", paddingRight:6, whiteSpace:"nowrap" }}>{rowVar}</td>
                  {corrMatrix.values[ri].map((val,ci) => {
                    const isUpper = ci>=ri;
                    return (
                      <td key={ci} style={{ width:52, height:42, textAlign:"center", background:isUpper?heatColor(val):"transparent", borderRadius:4, fontFamily:"monospace", fontSize:10, color:isUpper?(Math.abs(val)>0.4?"#fff":C.text):"transparent", fontWeight:Math.abs(val)>0.5?700:400 }}>
                        {isUpper ? val.toFixed(2) : ""}
                      </td>
                    );
                  })}
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      </Card>

      <Card>
        <ChartLabel>Key Correlation Insights</ChartLabel>
        <div className="grid-2">
          {[
            { title:"Communication items inter-correlated", val:"ρ ≈ 0.49–0.51", color:C.always, note:"Listening, Explanation, Respect, Time — all highly correlated, measure one construct" },
            { title:"Communication → Composite Score",      val:"ρ ≈ 0.72–0.80", color:C.always, note:"Each item strongly predicts Comm.Score — valid composite measure" },
            { title:"Communication → Satisfaction Ratings", val:"ρ ≈ 0.00–0.02", color:C.danger, note:"Essentially ZERO correlation — communication does not predict any satisfaction rating" },
            { title:"HC Rating (inverse, borderline)",      val:"ρ = −0.038",    color:C.accent, note:"Only borderline significant and in the WRONG direction" },
          ].map(item => (
            <div key={item.title} style={{ background:"#0f172a", borderRadius:10, padding:"14px 16px", borderLeft:`3px solid ${item.color}` }}>
              <div style={{ display:"flex", justifyContent:"space-between", marginBottom:6, flexWrap:"wrap", gap:6 }}>
                <div style={{ fontSize:13, fontWeight:600, color:C.text }}>{item.title}</div>
                <span style={{ fontSize:14, fontFamily:"monospace", fontWeight:800, color:item.color, whiteSpace:"nowrap" }}>{item.val}</span>
              </div>
              <div style={{ fontSize:12, color:C.textMuted }}>{item.note}</div>
            </div>
          ))}
        </div>
      </Card>
    </>
  );
}

// ══════════════════════════════════════════════════════════════════════════════
// MAIN APP
// ══════════════════════════════════════════════════════════════════════════════
export default function CAHPSDashboard() {
  const [tab, setTab] = useState("overview");

  const { data: listenData  } = useCSV("listening_dist.csv");
  const { data: ratingData  } = useCSV("mean_ratings.csv");
  const { data: infData     } = useCSV("inferential.csv");
  const { data: modData     } = useCSV("models.csv");
  const { data: corrCsvData } = useCSV("correlation.csv");

  return (
    <>
      <style>{STYLES}</style>
      <div style={{ minHeight:"100vh", background:C.bg, color:C.text }}>

        {/* Header */}
        <div className="header-pad" style={{ background:"linear-gradient(135deg, #0f172a 0%, #1e3a5f 100%)", borderBottom:`1px solid ${C.border}`, padding:"32px 24px 28px" }}>
          <div className="header-wrap">
            <div style={{ fontSize:11, letterSpacing:"0.15em", color:C.accent, textTransform:"uppercase", marginBottom:8 }}>
              CAHPS Health Plan Survey · n = 3,000
            </div>
            <h1 className="dash-title" style={{ margin:"0 0 8px", fontSize:28, fontWeight:800, letterSpacing:"-0.03em", lineHeight:1.2 }}>
              Statistical Analysis Dashboard
            </h1>
            <p style={{ margin:0, color:C.textMuted, fontSize:15, maxWidth:620 }}>
              "Patients who feel listened to by healthcare staff report higher satisfaction with their care"
            </p>
            <div style={{ marginTop:18, display:"inline-block", padding:"6px 16px", background:"#ef444422", border:"1px solid #ef444466", borderRadius:8, fontSize:13, color:"#fca5a5", fontWeight:600 }}>
              ✕ Statement NOT SUPPORTED — No significant relationship found
            </div>
          </div>
        </div>

        {/* Nav */}
        <div className="nav-pad" style={{ borderBottom:`1px solid ${C.border}`, padding:"0 24px", position:"sticky", top:0, background:C.bg, zIndex:10 }}>
          <div className="header-wrap">
            <div className="tabs-nav">
              {TABS.map(t => (
                <button key={t.id} className="tab-btn" onClick={() => setTab(t.id)} style={{
                  color: tab===t.id ? C.accent : C.textMuted,
                  borderBottom: `2px solid ${tab===t.id ? C.accent : "transparent"}`,
                }}>{t.label}</button>
              ))}
            </div>
          </div>
        </div>

        {/* Content */}
        <div className="content-wrap" style={{ paddingBottom:60 }}>
          {tab==="overview"    && <OverviewTab    infData={infData} />}
          {tab==="descriptive" && <DescriptiveTab listenData={listenData} ratingData={ratingData} />}
          {tab==="inferential" && <InferentialTab infData={infData} />}
          {tab==="predictive"  && <PredictiveTab  modData={modData} />}
          {tab==="heatmap"     && <HeatmapTab     corrCsvData={corrCsvData} />}
        </div>

        <div style={{ textAlign:"center", paddingBottom:24, fontSize:12, color:C.border }}>
          CAHPS Health Plan Survey Statistical Analysis · R v4.x · tidyverse · rstatix · caret
        </div>
      </div>
    </>
  );
}