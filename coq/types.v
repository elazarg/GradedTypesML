Require Import Coq.Arith.Arith.
Require Import Coq.Bool.Bool.
Require Import Coq.Relations.Relation_Definitions.
Require Import Coq.Relations.Relation_Operators.
Require Import Lia.

(* === Base atoms with decidable equality === *)
Parameter base : Set.
Axiom base_eq_dec : forall (b1 b2: base), {b1 = b2} + {b1 <> b2}.

Definition base_eq (b1 b2: base) : bool :=
  if base_eq_dec b1 b2 then true else false.

Lemma base_eq_true_iff : forall b1 b2, base_eq b1 b2 = true <-> b1 = b2.
Proof.
  intros b1 b2. unfold base_eq.
  destruct (base_eq_dec b1 b2); split; intros; congruence.
Qed.

Lemma base_eq_refl : forall b, base_eq b b = true.
Proof. intro b; apply base_eq_true_iff; reflexivity. Qed.

Lemma base_eq_sym : forall b1 b2, base_eq b1 b2 = base_eq b2 b1.
Proof.
  intros b1 b2. unfold base_eq.
  destruct (base_eq_dec b1 b2), (base_eq_dec b2 b1); auto; congruence.
Qed.

Lemma base_eq_trans : forall b1 b2 b3,
  base_eq b1 b2 = true -> base_eq b2 b3 = true -> base_eq b1 b3 = true.
Proof.
  intros b1 b2 b3 H12 H23.
  apply base_eq_true_iff in H12; apply base_eq_true_iff in H23; subst.
  apply base_eq_refl.
Qed.

(* === Graded types === *)
Inductive graded_type : Set :=
  | Bot
  | Base (b : base) (g : nat)
  | Any (g : nat)
  | Top.

(* Optional grade: only Base/Any have a finite grade *)
Definition grade_of (t : graded_type) : option nat :=
  match t with
  | Base _ g => Some g
  | Any g => Some g
  | _ => None
  end.

(* === Boolean equality for convenience === *)
Definition type_eq (t1 t2 : graded_type) : bool :=
  match t1, t2 with
  | Bot, Bot => true
  | Top, Top => true
  | Base b1 g1, Base b2 g2 => andb (base_eq b1 b2) (Nat.eqb g1 g2)
  | Any g1, Any g2 => Nat.eqb g1 g2
  | _, _ => false
  end.

Lemma type_eq_true_iff : forall t1 t2, type_eq t1 t2 = true <-> t1 = t2.
Proof.
  split; intros H.
  - destruct t1, t2; simpl in H; try discriminate; auto.
    + apply andb_true_iff in H as [Hb Hg].
      apply base_eq_true_iff in Hb. apply Nat.eqb_eq in Hg. subst. reflexivity.
    + apply Nat.eqb_eq in H. subst. reflexivity.
  - subst. destruct t2; simpl; try reflexivity.
    + rewrite base_eq_refl, Nat.eqb_refl; reflexivity.
    + apply Nat.eqb_refl.
Qed.

(* === Primitive generating relation (NO reflexivity here) === *)
Inductive substep : graded_type -> graded_type -> Prop :=
  | Sub_Bot       : forall b, substep Bot (Base b 0)
  | Sub_Top       : forall t, substep t Top
  | Sub_Base_Any  : forall b g, substep (Base b g) (Any g)
  | Sub_Any_Base  : forall b g, substep (Any g) (Base b (S g)).

(* Full subtyping = reflexive–transitive closure of substep *)
Definition Subtype : graded_type -> graded_type -> Prop :=
  clos_refl_trans _ substep.

(* === Grade monotonicity: grades never go down along Subtype (when both exist) === *)
Lemma substep_grade_monotone :
  forall t1 t2 g1 g2,
    substep t1 t2 ->
    grade_of t1 = Some g1 ->
    grade_of t2 = Some g2 ->
    g1 <= g2.
Proof.
  intros t1 t2 g1 g2 Hs H1 H2.
  destruct Hs; simpl in *.
  - discriminate. (* Bot -> Base 0 : source has no grade *)
  - discriminate. (* t -> Top : target has no grade *)
  - inversion H1; inversion H2; subst; lia.  (* Base g -> Any g: g <= g *)
  - inversion H1; inversion H2; subst; lia.  (* Any g -> Base (S g): g <= S g *)
Qed.

(* No path out of Top except reflexivity *)
Lemma subtype_from_top_only_refl : forall t, Subtype Top t -> t = Top.
Proof.
  intros t H. remember Top as s.
  induction H; subst; auto.
  - inversion H. reflexivity. (* no substep from Top *)
  - specialize (IHclos_refl_trans1 eq_refl). subst.
    specialize (IHclos_refl_trans2 eq_refl). assumption.
Qed.

(* No path to Bot except reflexivity *)
Lemma subtype_to_bot_only_refl : forall t, Subtype t Bot -> t = Bot.
Proof.
  intros t H. remember Bot as u.
  induction H; subst; auto.
  - inversion H.                            (* no substep to Bot *)
  - specialize (IHclos_refl_trans2 eq_refl). subst.
    specialize (IHclos_refl_trans1 eq_refl). assumption.
Qed.


(* Grades never decrease along Subtype, when both endpoints are graded *)
Lemma subtype_grade_monotone :
  forall t1 t2 g1 g2,
    Subtype t1 t2 ->
    grade_of t1 = Some g1 ->
    grade_of t2 = Some g2 ->
    g1 <= g2.
Proof.
  intros t1 t2 g1 g2 H.
  revert g1 g2.
  induction H; intros g1 g2 Hg1 Hg2.
  - inversion H; subst; inversion Hg1; inversion Hg2; subst.
    + constructor.
    + constructor. constructor.
  - rewrite -> Hg1 in Hg2.
    inversion Hg2.
    constructor.
  - destruct (grade_of y) as [gy|] eqn:Hgy.
    + apply IHclos_refl_trans1 with (g2:=gy) in Hg1.
      * apply IHclos_refl_trans2 with (g1:=gy) in Hg2; [lia|reflexivity].
      * reflexivity.
    + destruct y.
      * apply subtype_to_bot_only_refl in H. subst. inversion Hg1.
      * inversion Hgy.
      * inversion Hgy.
      * apply subtype_from_top_only_refl in H0. subst. inversion Hg2.
Qed.
